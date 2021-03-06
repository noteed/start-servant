{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Logging 
Description: Custom prelude for start-servant

We import Protolude and re-export it, for the usual protolude niceties;
and this module is meant to add more such re-exports as necessary as the project progresses.

-}
module Logging
  (
    -- * Text handling
    sentence
  , ellipsis
  -- ** Logging 
  , L.runLogT'
  , L.runLogT
  , L.localEnv
  , L.Logger
  , L.Level(..)
  , L.LogType(..)
  , L.simpleTimeFormat
  , L.makeDefaultLogger
  , L.TextShow(..)
  , L.MonadLog(..)
  -- *** Logging levels. 
  , L.levelDebug
  , L.levelInfo
  , L.levelCritical
  , L.levelError
  -- *** Loggers with proper sentence termination with periods.
  , debug
  , info
  , warning
  , error
  , critical
  , debugW
  , infoW
  , warningW
  , errorW
  , criticalW
  -- *** Loggers with proper sentence termination with ellipsis.
  , debugE
  , infoE
  , warningE
  , errorE
  , criticalE
  -- * Application name
  , AppName(..)
  , unAppName
  , showAppName
  , parseAppName
  -- * Logger
  , AppNameLogger
  -- * Utils
  , pShowStrict
  , pShowLazy
  ) where

import           Control.Lens
import qualified Control.Monad.Log             as L
import qualified Data.String                   as Str          -- required for IsString instance.
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Options.Applicative           as A
import "protolude" Protolude
import qualified Text.Pretty.Simple            as PS

-- | A type alias for convenience: this is a `L.Logger` where the `env` type is an `AppName`. 
type AppNameLogger = L.Logger AppName

-- | Terminate a sentence with a period; avoids clumsy mappends etc. for properly formatting sentences.
sentence :: Text -> Text
sentence = (`mappend` ".")

-- | Terminate a sentence with an ellipsis; avoids clumsy mappends etc. for properly formatting sentences.
ellipsis :: Text -> Text
ellipsis = (`mappend` "…")

-- Logging functions that log with properly terminated sentences.

debugE :: L.MonadLog env m => Text -> m ()
debugE = L.debug . ellipsis

infoE :: L.MonadLog env m => Text -> m ()
infoE = L.info . ellipsis

warningE :: L.MonadLog env m => Text -> m ()
warningE = L.warning . ellipsis

errorE :: L.MonadLog env m => Text -> m ()
errorE = L.error . ellipsis

criticalE :: L.MonadLog env m => Text -> m ()
criticalE = L.critical . ellipsis

debug :: L.MonadLog env m => Text -> m ()
debug = L.debug . sentence

info :: L.MonadLog env m => Text -> m ()
info = L.info . sentence

warning :: L.MonadLog env m => Text -> m ()
warning = L.warning . sentence

error :: L.MonadLog env m => Text -> m ()
error = L.error . sentence

critical :: L.MonadLog env m => Text -> m ()
critical = L.critical . sentence

debugW :: (L.MonadLog env m, Foldable f) => f Text -> m ()
debugW = debug . T.unwords . toList

infoW :: (L.MonadLog env m, Foldable f) => f Text -> m ()
infoW = info . T.unwords . toList

warningW :: (L.MonadLog env m, Foldable f) => f Text -> m ()
warningW = warning . T.unwords . toList

errorW :: (L.MonadLog env m, Foldable f) => f Text -> m ()
errorW = error . T.unwords . toList

criticalW :: (L.MonadLog env m, Foldable f) => f Text -> m ()
criticalW = critical . T.unwords . toList

-- | An application name: lets us group logging etc. with @/@ as separators.
newtype AppName = AppName { _unAppName :: [Text] }
                deriving (Eq, Show, Semigroup, Monoid) via [Text]

instance L.TextShow AppName where
  showb = L.showb . showAppName

-- | Reverse of the IsString instance (below)
showAppName (AppName envs) = T.intercalate "/" envs

makeLenses ''AppName

-- | Take any string; split at @/@; and use it as the AppName.
instance IsString AppName where
  fromString = appNameFromText . T.pack

appNameFromText :: Text -> AppName
appNameFromText = AppName . T.splitOn "/"

instance StringConv Text AppName where
  strConv _ = appNameFromText

instance StringConv Str.String AppName where
  strConv _ = appNameFromText . T.pack

-- | Parse the application name (`AppName`) wherein the sections are separated by @/@.
-- Note the use of fromString which ensures we split out the incoming string properly.
parseAppName :: A.Parser AppName
parseAppName = Str.fromString <$> A.strOption
  (A.long "logging-root-app-name" <> A.metavar "STRING" <> A.help
    "Application name: sections separated by `/`"
  )

-- | The default pretty show expects lazy text, which is not compatible with our logging.
pShowStrict :: Show a => a -> Text
pShowStrict = TL.toStrict . PS.pShow
{-# INLINE pShowStrict #-}

-- | Renamed version of the pShow from the pretty-simple lib.
pShowLazy :: Show a => a -> TL.Text
pShowLazy = PS.pShow
{-# INLINE pShowLazy #-}
