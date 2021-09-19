{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{- |
Module: Prototype.Server.New
Description: Server implementation for the new servers

General, and mode-specific servers and hoists, respectively.

For servers, we keep the implementations general: we never mention /what/ an @m@ is; we just constrain the @m@. 

-}
module Prototype.Server.New.StartPage
  ( -- * Unauthd. part
    publicT
  , Public
  -- ** $loginConstraints
  , LoginC
  -- * Authd. part. 
  , protectedT
  , Protected
  -- * Protected constraints
  , ProtectedC
  ) where

import           Control.Lens
import qualified Prototype.Runtime             as Rt
import qualified Prototype.Runtime.Errors      as Rt
import qualified Prototype.Runtime.Storage     as S
import qualified Prototype.Server.New.Auth     as Auth
import           Prototype.Server.New.Page
import qualified Prototype.Server.New.Page.UserPages
                                               as UP
import qualified Prototype.Server.New.Todos    as Todos
import           Prototype.Types
import           Servant.API
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import           Servant.Server
import           Servant.Server.StaticFiles     ( serveDirectoryFileServer )

-- brittany-disable-next-binding 
-- | The public set of pages;
-- TODO: forgot password pages etc. 
type Public = "public" :>
  ( -- Ask the user to login 
    "login" :> ( Get '[B.HTML] (Page 'Public LoginPage)
                 :<|> "authenticate" -- actually authenticate the user. 
                      :> ReqBody '[FormUrlEncoded] Credentials
                      :> Verb 'POST 303 '[JSON] ( Headers Auth.PostAuthHeaders
                                                  NoContent
                                                )
               )
    :<|> "signup" :> Get '[B.HTML] (Page 'Public SignupPage)
    :<|> "static" :> Raw
  )

-- $loginConstraints Constraints needed for logging users in
type LoginC mode m
  = ( MonadReader (Rt.Runtime mode) m
    , MonadLog Rt.AppName m
    , MonadError Rt.RuntimeErr m
    , S.DBStorage m User
    , MonadIO m
    )

-- | UnprotectedT server: provide routes that need no user authentication.
publicT :: forall mode m . LoginC mode m => ServerT Public m
publicT =
  (showLoginPage :<|> userLogin)
    :<|> showSignupPage
    :<|> (serveDirectoryFileServer "static/")
 where
  showLoginPage  = pure . PublicPage $ LoginPage "/public/login/authenticate"
  showSignupPage = pure $ PublicPage SignupPage
  userLogin creds@Credentials {..} =
    S.dbSelect (AuthUser creds) >>= maybe unauthdErr authdCookie . headMay
   where
    authdCookie user = do
      -- get the config. to get the cookie and JWT settings. 
      Rt.Conf {..}  <- asks Rt._rConf
      jwtSettings   <- asks Rt._rJwtSettings
      mApplyCookies <- liftIO
        $ SAuth.acceptLogin _cCookieSettings jwtSettings user
      case mApplyCookies of
        Nothing           -> unauthdErr
        Just applyCookies -> do
          info "User logged in"
          pure . addHeader @"Location" "/private/welcome" $ applyCookies
            NoContent

    unauthdErr = Rt.throwError' . AuthFailed . show $ username

-- brittany-disable-next-binding
type Protected = Auth.UserAuthentication :> UserPages

-- brittany-disable-next-binding
type UserPages =
  -- User's welcome screen. 
  "private" :> ( "welcome" :> Get '[B.HTML] (Page 'Authd Profile)
               :<|> "user" :> ( "groups" :> Get '[B.HTML] (Page 'Authd UP.UserGroups)
                           :<|> "todos"  :> Todos.Todos -- The todos API
                              )
               )

type ProtectedC m
  = (Applicative m, MonadError Rt.RuntimeErr m, S.DBStorage m TodoList)

-- | Server for authenticated users. 
protectedT :: forall m . ProtectedC m => ServerT Protected m
protectedT (SAuth.Authenticated authdUser@User {..}) =
  startPage :<|> (showUserGroups :<|> Todos.todosT authdUser)
 where
  showUserGroups = pure . AuthdPage authdUser $ UP.UserGroups userGroups
  startPage      = pure . AuthdPage authdUser $ Profile
    { namespace   = authdUser ^. uUsername
    , email       = email
    , name        = "TODO" -- TODO 
    , profGroups  = userGroups
    , profTagRels = authdUser ^. uUserTagRels
    }
protectedT authFailed = dispErr :<|> dispErr :<|> dispErr :<|> (const dispErr {- move to Todo module -}
                                                                             )
 where
  dispErr :: forall m' a . MonadError Rt.RuntimeErr m' => m' a
  dispErr = Rt.throwError' . AuthFailed $ show authFailed

