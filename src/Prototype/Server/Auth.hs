{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype.Server.Auth where

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar, TVar)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server
import Web.FormUrlEncoded (FromForm)

import qualified Prototype.Database as Database
import Prototype.Types (authenticateProfile, Credentials, Profile(..), User(..))


--------------------------------------------------------------------------------
-- This is the login handler. We create a cookie with a User content, signed by
-- our key. Later, in a Auth protected route, we can retrieve it.
checkCredentials :: Database.Handle -> CookieSettings -> JWTSettings -> Credentials
  -> Handler (Headers '[ Header "Location" String
                       , Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie]
                      NoContent)
checkCredentials database cookieSettings jwtSettings credentials = do
  muser <- liftIO . atomically $ do
    profiles <- readTVar (Database.hUsers database)
    case authenticateProfile credentials profiles of
      Just Profile {..} -> do
        Database.addSession database namespace
        let user = User namespace email
        return (Just user)
      Nothing -> return Nothing

  case muser of
    Just user -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Just applyCookies ->
          -- Redirect to homepage after succesful login.
          return $ addHeader "/" $ applyCookies NoContent
        Nothing -> throwError err401

    Nothing -> throwError err401
