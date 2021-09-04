{-# LANGUAGE OverloadedStrings #-} -- For hard-coded key.

module Main (main) where

import qualified Control.Monad.Log as L 
import qualified Parse 
import qualified Prototype.Runtime as Rt 
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

-- For hard-coded key.
import Crypto.JOSE.JWK (fromKeyMaterial)
import Crypto.JOSE.JWA.JWK (KeyMaterial(OctKeyMaterial), OctKeyParameters(..))
import Crypto.JOSE.Types (Base64Octets(..))

import qualified Options.Applicative as A 
import Prototype.Server (api, server)


--------------------------------------------------------------------------------
port :: Int
port = 7249


--------------------------------------------------------------------------------
main :: IO ()
main = do
  confInMode <- A.execParser Parse.parseFullConf 
  putStrLn @Text $ "Starting up: " <> show confInMode 

  -- TODO: Handle the error case 
  case confInMode of
    Parse.ConfStmMode conf -> stmLifecycle conf
    -- TODO add others. 
    _ -> putStrLn @Text "Only STM mode supported for now!" >> exitFailure

stmLifecycle conf = do
  Right runtime@Rt.Runtime {..} <- Rt.bootStm conf
  L.runLogT' _rLogger $ do  
    -- This could be taken form disk, or hard-coded here, to keep existing
    -- sessions alive.
    key <- liftIO $ generateKey
    -- let key = fromKeyMaterial (OctKeyMaterial (OctKeyParameters (Base64Octets "aa")))

    -- Disable XSRF Cookie (otherwise, this needs some logic instead of
    -- simple cURL calls):
    -- https://github.com/haskell-servant/servant-auth/issues/55#issuecomment-747046527
    -- Using Secure with Same-Site=Strict would be good enough ?
    let cookieSettings = defaultCookieSettings
          { cookieIsSecure = NotSecure
            -- ^ Use temporarily NotSecure for easier local testing with cURL.
          , cookieXsrfSetting = Nothing
          , cookieSameSite = SameSiteStrict
          }

        jwtSettings = defaultJWTSettings key

        settings = cookieSettings :. jwtSettings :. EmptyContext

    -- Run the server enclosed within a `try` so we can handle all exits, and gracefully shut down
    -- storage, etc. 
    exitReason <- liftIO . try @SomeException . run port $ serveWithContext api settings $
      server _rStorage cookieSettings jwtSettings
    logExit runtime exitReason
  where
    -- Log and exit; in the future, we'd like to use the runtime to gracefully shut down storage.
    -- Eg. if we are writing our STM State to disk using a Read-Show to persist state between
    -- restarts. Or for DB connection pools, where we'd like to shutdown all connections,
    -- gracefully committing pending transactions etc. 
    logExit _ = \case
      Left err -> L.error ("Exiting on error: "  <> show err)  >> liftIO exitFailure 
      Right{} -> L.info "Exiting without any issues." >> liftIO exitSuccess
