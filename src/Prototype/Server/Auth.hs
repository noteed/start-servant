{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Prototype.Server.Auth (login) where

import Control.Concurrent.STM (atomically)
import Control.Monad.Trans (liftIO)
import Servant
import Servant.Auth.Server

import qualified Prototype.Runtime.StmDatabase as Database
import Prototype.Types (Credentials)


--------------------------------------------------------------------------------
-- This is the login handler. We create a cookie with a User content, signed by
-- our key. Later, in a Auth protected route, we can retrieve it.
login :: Database.Handle -> CookieSettings -> JWTSettings -> Credentials
  -> Handler (Headers '[ Header "Location" Text
                       , Header "Set-Cookie" SetCookie
                       , Header "Set-Cookie" SetCookie]
                      NoContent)
login database cookieSettings jwtSettings credentials = do
  muser <- liftIO . atomically $ Database.login database credentials
  case muser of
    Just user -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Just applyCookies ->
          -- Redirect to homepage after succesful login.
          return $ addHeader "/" $ applyCookies NoContent
        Nothing -> throwError err401

    Nothing -> throwError err401
