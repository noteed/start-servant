{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-} -- For hard-coded key.

module Main (main) where

import Prototype.Module
import qualified Prototype.Runtime.Errors as Errs
import Control.Monad.Fail (MonadFail) -- needed for pattern matching on LHS inside do blocks.
import qualified Data.Text as T
import qualified Control.Monad.Log as L
import qualified Parse
import qualified Prototype.Runtime as Rt
import Network.Wai.Handler.Warp (run)
import Servant
import qualified Servant.Auth.Server as Srv

import qualified Options.Applicative as A
import Prototype.Server.Legacy (api, server)


--------------------------------------------------------------------------------
port :: Int
port = 7249

--------------------------------------------------------------------------------
main :: IO ()
main = do
  confInMode <- A.execParser Parse.parseFullConf
  outputConfAndModule confInMode
  -- TODO: Handle the error case
  case confInMode of
    Parse.ConfStmMode conf -> stmLifecycle conf
    -- TODO add others.
    _ -> putStrLn @Text "Only STM mode supported for now!" >> exitFailure
  where

    outputConfAndModule conf = do
      -- Get the information about the current module using some top level "value" in the module.
      let currentModule = moduleOf 'main
      putStrLn @Text $ "Current module: " <> show currentModule
      outputConf conf

    outputConf = putStrLn . \case
      Parse.ConfStmMode conf ->
        T.unlines [ "STM-Mode:" , show conf ]
      Parse.ConfPostgresMode conf ->
        T.unlines [ "Postgres-Mode:" , show conf ]

stmLifecycle :: (MonadIO m, MonadFail m) => Rt.Conf -> m ()
stmLifecycle conf@Rt.Conf{ _cServerMode } = case _cServerMode of
  Rt.New -> undefined
  Rt.Legacy -> stmBootAndLifecycle stmLegacyLifecycle conf

stmBootAndLifecycle :: MonadIO m => (Rt.StmRuntime -> m ()) -> Rt.Conf -> m ()
stmBootAndLifecycle lifecycle conf@Rt.Conf {..} = do
  key <- liftIO Srv.generateKey
  Rt.bootStm key conf >>= either reportErr lifecycle
  where
    reportErr = putStrLn @Text . mappend "Unable to boot: " . Errs.displayErr

stmLegacyLifecycle :: (MonadIO m, MonadFail m) => Rt.StmRuntime -> m ()
stmLegacyLifecycle runtime@Rt.Runtime{..} = do
  L.runLogT' _rLogger $ do

    let settings = _cCookieSettings :. _rJwtSettings :. EmptyContext
    -- Run the server enclosed within a `try` so we can handle all exits, and gracefully shut down
    -- storage, etc.
    exitReason <- liftIO . try @SomeException . run port $ serveWithContext api settings $
      server _rStorage _cCookieSettings _rJwtSettings
    logExit runtime exitReason
  where
    Rt.Conf {..} = _rConf

-- Log and exit; in the future, we'd like to use the runtime to gracefully shut down storage.
-- Eg. if we are writing our STM State to disk using a Read-Show to persist state between
-- restarts. Or for DB connection pools, where we'd like to shutdown all connections,
-- gracefully committing pending transactions etc.
logExit rt exitStatus = wrapUpRuntime rt >> case exitStatus of
  Left err -> error ("Exiting on error: "  <> show err)  >> liftIO exitFailure
  Right{} -> info "Exiting without any issues" >> liftIO exitSuccess
  where
    wrapUpRuntime Rt.Runtime{..} = liftIO $ L.cleanUp _rLogger
