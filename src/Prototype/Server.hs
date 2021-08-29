{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Prototype.Server ( api
                        , server
                        , protected
                        , Protected
                        , API
                        ) where

import Control.Concurrent.STM (atomically)
import Control.Monad.Trans (liftIO)
import Servant
import Servant.Auth.Server
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html5 (Html)

import qualified Prototype.Runtime.Database as Database
import Prototype.Html
  ( databaseIndex, document, document', loginPage, namespaceIndex
  , profilePage, todoListIndex)
  -- And also for ToMarkup instances.
import qualified Prototype.Pages.Home as Pages
import Prototype.Server.Auth
import Prototype.Types



--------------------------------------------------------------------------------
type API =
       (Servant.Auth.Server.Auth '[Cookie] User :> Protected)
  :<|> Unprotected

api :: Proxy API
api = Proxy

server :: Database.Handle -> CookieSettings -> JWTSettings -> Server API
server database cookieSettings jwtSettings =
       protected database
  :<|> unprotected database cookieSettings jwtSettings


--------------------------------------------------------------------------------
type Protected =
       Get '[HTML] Html
  :<|> "login" :> Get '[HTML] Html
  :<|> "settings" :> "profile" :> Get '[HTML] Html
  :<|> "settings" :> "profile" :> "username" :> Get '[JSON] Namespace
  :<|> "settings" :> "profile" :> "email" :> Get '[JSON] Text

  :<|> "a" :> "counter" :> Get '[JSON] Int
  :<|> "a" :> "bump" :> Verb 'POST 204 '[JSON] NoContent

  :<|> "a" :> "settings" :> "profile" :> Get '[JSON, HTML] Profile

  :<|> "a" :> "sessions" :> Get '[JSON] [Session]
  :<|> "a" :> "profiles" :> Get '[JSON] [Profile]
  :<|> "a" :> "lists" :> Get '[JSON] [TodoList]

  :<|> "database" :> Get '[HTML] Html

  :<|> Capture "namespace" Namespace :> Get '[HTML] Html
  :<|> Capture "namespace" Namespace :> Capture "list" Text :> Get '[HTML] Html

-- 'Protected' will be protected by 'auths', which we still have to specify.
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
-- This is also used for routes that can rendered differently when a user is
-- logged in or not.
-- TODO Is is possible to get a Profile, instead of a User ?
protected :: Database.Handle -> Servant.Auth.Server.AuthResult User -> Server Protected
protected database result =
  (case result of
    Servant.Auth.Server.Authenticated user -> do
      mprofile <- liftIO . atomically $ Database.getLoggedInProfile database user
      case mprofile of
        Just profile ->
          return $ document (Just profile) "start-servant" $ Pages.homePage (Just profile)
        _ -> throwAll err404
             -- ^ If the user is authenticated, a profile must exists, so
             --  TODO we must log this case properly.
    _ ->
      return $ document Nothing "start-servant" $ Pages.homePage Nothing
  )
  :<|>
  (case result of
    Servant.Auth.Server.Authenticated user -> do
      mprofile <- liftIO . atomically $ Database.getLoggedInProfile database user
      case mprofile of
        Just profile ->
          return $ document' "start-servant" $ loginPage (Just profile)
        _ -> throwAll err404
             -- ^ If the user is authenticated, a profile must exists, so
             --  TODO we must log this case properly.
    _ ->
      return $ document' "start-servant" $ loginPage Nothing
  )
  :<|>
  case result of
    Servant.Auth.Server.Authenticated user ->
      (do
        mprofile <- liftIO . atomically $ Database.getLoggedInProfile database user
        case mprofile of
          Just profile ->
            return $ document mprofile "start-servant" $ profilePage profile
          _ -> throwAll err404
               -- ^ If the user is authenticated, a profile must exists, so
               --  TODO we must log this case properly.
      )
      :<|> return (username (user :: User))
      :<|> return (email (user :: User))
      :<|> getCounter database
      :<|> bumpCounter database
      :<|> (do
        mprofile <- liftIO . atomically $ Database.getLoggedInProfile database user
        case mprofile of
          Just profile -> return profile
          _ -> throwAll err404
               -- ^ If the user is authenticated, a profile must exists, so
               --  TODO we must log this case properly.
      )
      :<|> getSessions database
      :<|> getProfiles database
      :<|> getAllTodoLists database
      :<|> (do
        mprofile <- liftIO . atomically $ Database.getLoggedInProfile database user
        case mprofile of
          Just profile ->
            return $ document (Just profile) "start-servant" databaseIndex
          _ -> throwAll err404
      )
      :<|> (\namespace' -> do
        -- TODO ^ Validate the namespace, maybe create a custom Capture type ?
        mprofileAndLists <- liftIO . atomically $
          Database.getProfileAndLists database namespace'

        case mprofileAndLists of
          Just (profile, lists) ->
            return $ document (Just profile) "start-servant" $ namespaceIndex profile lists
          Nothing -> throwAll err404)
      :<|> (\namespace' listname -> do
        -- TODO ^ Validate the namespace, maybe create a custom Capture type ?
        mprofileAndList <- liftIO . atomically $
          Database.getProfileAndList database namespace' listname

        case mprofileAndList of
          Just (profile, list') ->
            return $ document (Just profile) "start-servant" $ todoListIndex profile list'
          Nothing -> throwAll err404)
    _ -> throwAll err401

getCounter database = do
  Counter i <- liftIO . atomically $ Database.getCounter database
  return i

bumpCounter database = do
  let op = BumpCounter
  liftIO . atomically $ Database.apply database op
  return NoContent

getSessions database =
  liftIO . atomically $ Database.getSessions database

getProfiles database =
  liftIO . atomically $ Database.getProfiles database

getAllTodoLists database = do
  xs <- liftIO . atomically $ Database.getAllTodoLists database
  return (map snd xs)


--------------------------------------------------------------------------------
type Unprotected =
       "login"
         :> ReqBody '[FormUrlEncoded] Credentials
         :> Verb 'POST 303 '[JSON] (Headers '[ Header "Location" Text
                                             , Header "Set-Cookie" SetCookie
                                             , Header "Set-Cookie" SetCookie]
                                            NoContent)
  :<|> "static" :> Raw

-- 'Unprotected' is used for un-authenticated routes, which include in
-- particular the route to get logged in.
unprotected :: Database.Handle -> CookieSettings -> JWTSettings -> Server Unprotected
unprotected database cs jwts =
       login database cs jwts
  :<|> serveDirectoryFileServer "static/"
       -- ^ This presents an index of available files within the directory.
