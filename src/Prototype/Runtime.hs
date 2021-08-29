module Prototype.Runtime
  () where

import           Control.Monad.Log             as L

newtype Conf = Conf
  { _cAppName :: Text
  }
  deriving (Eq, Show)
