{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module: Prelude
Description: Custom prelude for start-servant

We import Protolude and re-export it, for the usual protolude niceties;
and this module is meant to add more such re-exports as necessary as the project progresses.

-}
module Prelude
  (
    -- * Text handling
    sentence
  , ellipsis
  -- * Re-exports
  , module Proto
  , module Def
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
  ) where

import           Control.Lens
import qualified Control.Monad.Log             as L
import           Data.Default.Class            as Def
import qualified Data.String     -- required for IsString instance.
import qualified Data.Text                     as T
import           Protolude                     as Proto

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
  fromString = AppName . T.splitOn "/" . T.pack

