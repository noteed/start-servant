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
    unprotectedT
  -- Another re-export exception; we want the parent modules to use the `Protected` being
  -- exported from this module. This will ensure if we swap this type, parent modules have these changes too.
  , L.Unprotected
  -- ** $loginConstraints
  , LoginC
  -- * Authd. part. 
  , protectedT
  , Protected
  ) where

import           Control.Lens
import qualified Prototype.Runtime             as Rt
import qualified Prototype.Runtime.Errors      as Rt
import qualified Prototype.Runtime.Storage     as S
import qualified Prototype.Server.Legacy       as L
import           Prototype.Server.New.Page
import           Prototype.Types
import           Servant.API
import qualified Servant.Auth.Server           as SAuth
import qualified Servant.HTML.Blaze            as B
import           Servant.Server
import           Servant.Server.StaticFiles     ( serveDirectoryFileServer )

-- $loginConstraints Constraints needed for logging users in
type LoginC mode m
  = ( MonadReader (Rt.Runtime mode) m
    , MonadLog Rt.AppName m
    , MonadError Rt.RuntimeErr m
    , S.DBStorage m User
    , MonadIO m
    )

-- | UnprotectedT server: provide routes that need no user authentication.
unprotectedT :: forall mode m . LoginC mode m => ServerT L.Unprotected m
unprotectedT = userLogin :<|> serveDirectoryFileServer "static/"
 where
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
        Nothing -> unauthdErr
        Just applyCookies ->
          pure . addHeader @"Location" "/start" $ applyCookies NoContent

    unauthdErr = Rt.throwError' . AuthFailed . show $ username

-- brittany-disable-next-binding
type Protected = SAuth.Auth '[SAuth.Cookie] User :> UserPages

-- brittany-disable-next-binding
type UserPages =
  -- User's welcome screen. 
  "start" :> Get '[B.HTML] (Page 'Authd Profile)

-- | Server for authenticated users. 
protectedT
  :: (Applicative m, MonadError Rt.RuntimeErr m) => ServerT Protected m
protectedT (SAuth.Authenticated authdUser@User {..}) = startPage
 where
  startPage = pure . AuthdPage authdUser $ Profile
    { namespace   = authdUser ^. uUsername
    , email       = email
    , name        = "TODO" -- TODO 
    , profGroups  = userGroups
    , profTagRels = authdUser ^. uUserTagRels
    }
protectedT authFailed = Rt.throwError' . AuthFailed $ show authFailed
