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
  ( unprotected
  ) where

import qualified Prototype.Runtime             as Rt
import qualified Prototype.Runtime.Errors      as Rt
import qualified Prototype.Runtime.Storage     as S
import qualified Prototype.Server.Legacy       as L
import           Prototype.Types
import           Servant.API
import qualified Servant.Auth.Server           as SAuth
import           Servant.Server
import           Servant.Server.StaticFiles     ( serveDirectoryFileServer )

-- | Unprotected server: provide routes that need no user authentication.
unprotected
  :: forall mode m
   . ( MonadReader (Rt.Runtime mode) m
     , MonadLog Rt.AppName m
     , MonadError Rt.RuntimeErr m
     , S.DBStorage m User
     , MonadIO m
     )
  => ServerT L.Unprotected m
unprotected = userLogin :<|> serveDirectoryFileServer "static/"
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
          pure . addHeader @"Location" "/" $ applyCookies NoContent

    unauthdErr = Rt.throwError' . AuthFailed . show $ username

