{-# LANGUAGE OverloadedStrings #-} -- For hard-coded key.

module Main (main) where

import qualified Data.Text as T
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
  outputConf confInMode
  -- TODO: Handle the error case
  case confInMode of
    Parse.ConfStmMode conf -> stmLifecycle conf
    -- TODO add others.
    _ -> putStrLn @Text "Only STM mode supported for now!" >> exitFailure
  where
    outputConf = putStrLn . \case
      Parse.ConfStmMode conf ->
        T.unlines [ "STM-Mode:" , show conf ]
      Parse.ConfPostgresMode conf ->
        T.unlines [ "Postgres-Mode:" , show conf ]

stmLifecycle conf@Rt.Conf{..} = do
  Right runtime@Rt.Runtime {..} <- Rt.bootStm conf
  L.runLogT' _rLogger $ do
    -- This could be taken form disk, or hard-coded here, to keep existing
    -- sessions alive.
    key <- liftIO generateKey
    -- let key = fromKeyMaterial (OctKeyMaterial (OctKeyParameters (Base64Octets "aa")))

    let jwtSettings = _cMkJwtSettings key
        settings = _cCookieSettings :. jwtSettings :. EmptyContext

    -- Run the server enclosed within a `try` so we can handle all exits, and gracefully shut down
    -- storage, etc.
    exitReason <- liftIO . try @SomeException . run port $ serveWithContext api settings $
      server _rStorage _cCookieSettings jwtSettings
    logExit runtime exitReason
  where
    -- Log and exit; in the future, we'd like to use the runtime to gracefully shut down storage.
    -- Eg. if we are writing our STM State to disk using a Read-Show to persist state between
    -- restarts. Or for DB connection pools, where we'd like to shutdown all connections,
    -- gracefully committing pending transactions etc.
    logExit _ = \case
      Left err -> L.error ("Exiting on error: "  <> show err)  >> liftIO exitFailure
      Right{} -> L.info "Exiting without any issues." >> liftIO exitSuccess
