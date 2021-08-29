module Prototype.Runtime
  () where

import           Control.Monad.Log             as L
import           Prelude                 hiding ( Handle )
import           Prototype.Runtime.StmDatabase    as Db

newtype Conf = Conf
  { _cAppName :: Text
  }
  deriving (Eq, Show)

-- | STM based runtime
data StmRuntime = StmRuntime
  { _srConf    :: Conf
  , _srStorage :: Handle
  }
