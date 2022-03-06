{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module MultiLogging
  (
  -- * Logging configuration
    LoggingConf(..)
  -- ** Configuration lenses. 
  , lcOutputs
  , lcRootAppName
  , lcBufSize
  , lcLogLevel
  -- ** Parsing the configuration from the CLI.
  , parseLoggingConf

  -- * Multiple loggers in a type-safe newtype. 
  , AppNameLoggers(..)
  -- ** Lenses 
  , appNameLoggers

  -- * Instantiate loggers. 
  , makeDefaultLoggersWithConf
  -- * Flush and close the loggers, eg. at the end of an application lifecycle. 
  , flushAndCloseLoggers

  -- * Typeclass for monads that support logging over multiple loggers. 
  , MonadAppNameLogMulti(..)

  -- * Common logging functions over multiple loggers. 
  , debug
  , info
  , warning
  , error
  , critical

  -- * Common logging functions over multiple loggers, with explicit AppName tags. 
  , debugEnv
  , infoEnv
  , warningEnv
  , errorEnv
  , criticalEnv

  -- * Re-exports for convenience.
  , L.AppName(..)
  ) where

import           Control.Lens
import           Control.Monad.Catch            ( MonadMask )
import qualified Control.Monad.Log             as ML
import qualified Logging                       as L
import qualified Options.Applicative           as A
import           Protolude
import qualified System.Log.FastLogger         as FL
import qualified System.Log.FastLogger.File    as FL.File

-- | Configuration for logging.
data LoggingConf = LoggingConf
  { _lcOutputs     :: [FL.LogType] -- ^ Multiple logging outputs: logging is disabled if this list is empty.
  , _lcRootAppName :: L.AppName  -- ^ Application name to use at the root of the logger. 
  , _lcBufSize     :: FL.BufSize -- ^ Buffer size to observe for logging.
  , _lcLogLevel    :: L.Level -- ^ The min. logging level to output. Any logging under this level will be filtered out. 
  }

-- | Parse the logging configuration from the CLI.
parseLoggingConf :: A.Parser LoggingConf
parseLoggingConf = do
  _lcOutputs     <- A.many parseLogType
  _lcRootAppName <- L.parseAppName
  _lcBufSize     <- bufSize
  _lcLogLevel    <- logLevel
  pure LoggingConf { .. }

logLevel =
  A.option (A.eitherReader readEither)
    $  A.long "logging-level"
    <> A.value ML.levelInfo
    <> A.showDefault
    <> A.metavar "LOGGING_LEVEL"

parseLogType :: A.Parser FL.LogType
parseLogType = off <|> logStdout <|> logStderr <|> fileNoRot <|> file

off = A.flag' FL.LogNone (A.long "logging-off" <> A.help "Turn logging off.")
logStdout = FL.LogStdout <$> bufSize
logStderr = FL.LogStderr <$> bufSize
fileNoRot = FL.LogFileNoRotate <$> fpath <*> bufSize
file = FL.LogFile <$> logSpec <*> bufSize
logSpec = FL.File.FileLogSpec <$> fpath <*> fsize <*> fbackupNumber
fsize =
  fmap abs . A.option A.auto $ A.long "logging-file-size" <> A.metavar "BYTES"
fbackupNumber =
  fmap abs
    .  A.option A.auto
    $  A.long "logging-file-backup-number"
    <> A.help "The backup number of the logging file."
    <> A.metavar "INT"

fpath =
  A.strOption $ A.long "logging-file-name" <> A.help "Path to the logging file."
bufSize =
  A.option A.auto
    $  A.long "logging-buf-size"
    <> A.metavar "BYTES"
    <> A.value FL.defaultBufSize
    <> A.showDefault

-- | A set of multiple loggers. 
newtype AppNameLoggers = AppNameLoggers { _appNameLoggers :: [L.AppNameLogger]
                                        -- ^ Multiple loggers to log over. 
                                        }

makeLenses ''AppNameLoggers

-- | Generate loggers with a given configuration: generates separate loggers for each LogType specified in the config. 
makeDefaultLoggersWithConf :: MonadIO m => LoggingConf -> m AppNameLoggers
makeDefaultLoggersWithConf LoggingConf {..} =
  AppNameLoggers <$> mapM defaultLoggerFor _lcOutputs
 where
  defaultLoggerFor logOut =
    L.makeDefaultLogger L.simpleTimeFormat logOut _lcLogLevel _lcRootAppName

-- | Clean-up & close the loggers. 
flushAndCloseLoggers :: MonadIO m => AppNameLoggers -> m ()
flushAndCloseLoggers (AppNameLoggers loggers) =
  liftIO $ mapM_ ML.cleanUp loggers

makeLenses ''LoggingConf

-- | A Monad for logging over a collection of logs. 
class MonadAppNameLogMulti m where
  -- | Get the current set of loggers. 
  askLoggers :: m AppNameLoggers
  -- | Locally modified loggers, useful for localised logging envs. 
  localLoggers :: (L.AppNameLogger -> L.AppNameLogger) -> m a -> m a

-- | A unified set of minimal constraints required for us to be able to log over multiple loggers. 
type LoggingConstraints m = (MonadIO m, MonadMask m, MonadAppNameLogMulti m)

debug :: LoggingConstraints m => Text -> m ()
debug = runLogFuncMulti . ML.debug

info :: LoggingConstraints m => Text -> m ()
info = runLogFuncMulti . ML.info

warning :: LoggingConstraints m => Text -> m ()
warning = runLogFuncMulti . ML.warning

error :: LoggingConstraints m => Text -> m ()
error = runLogFuncMulti . ML.error

critical :: LoggingConstraints m => Text -> m ()
critical = runLogFuncMulti . ML.critical

debugEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
debugEnv name = runLogFuncMulti . ML.debug' name

infoEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
infoEnv name = runLogFuncMulti . ML.info' name

warningEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
warningEnv name = runLogFuncMulti . ML.warning' name

errorEnv :: LoggingConstraints m => L.AppName -> Text -> m ()
errorEnv name = runLogFuncMulti . ML.error' name

criticalEnv :: LoggingConstraints m => Text -> m ()
criticalEnv = runLogFuncMulti . ML.debug

runLogFuncMulti :: LoggingConstraints m => ML.LogT L.AppName m () -> m ()
runLogFuncMulti logFunc = do
  AppNameLoggers loggers <- askLoggers
  mapM_ runLoggerOver loggers
  where runLoggerOver logger = ML.runLogTSafe logger logFunc