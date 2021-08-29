{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE
    KindSignatures
  , DataKinds
  , TypeFamilies
#-}
module Prototype.Runtime
  ( -- * Configuration
    Conf(..)
  , cAppName
  , cLogLevel
  , AppMode(..)
  , AppName(..)
  , showAppName
  , unAppName
  , ModeStorage
  -- * Runtime
  , Runtime(..)
  , rConf
  , rStorage
  , rLogger
  -- * Control
  , AppM(..)
  ) where

import           Control.Lens
import           Control.Monad.Log             as L
import qualified Data.String                   as Str
import qualified Data.Text                     as T
import           Prelude                 hiding ( Handle )
import           Prototype.Runtime.Errors
import           Prototype.Runtime.StmDatabase as Db

-- | An application name: lets us group logging etc. with @/@ as separators.
newtype AppName = AppName { _unAppName :: [Text] }
                deriving (Eq, Show, Semigroup, Monoid) via [Text]

-- | Reverse of the IsString instance (below)
showAppName (AppName envs) = T.intercalate "/" envs

makeLenses ''AppName

-- | Take any string; split at @/@; and use it as the AppName.
instance IsString AppName where
  fromString = AppName . T.splitOn "/" . T.pack

data Conf = Conf
  { _cAppName  :: AppName -- ^ Application name
  , _cLogLevel :: L.Level -- ^ The logging level
  }
  deriving (Eq, Show)

makeLenses ''Conf

data AppMode = Stm | Postgres
             deriving (Eq, Show)

-- | A Type level function that gives us the kind of storage
-- based on the mode of the application.
type family ModeStorage (mode :: AppMode) :: Type where
  ModeStorage 'Stm = Handle
  -- Later: added as an example
  -- ModeStorage 'Postres = ConnectionPool

-- | STM based runtime
data Runtime (mode :: AppMode) = Runtime
  { _rConf    :: Conf -- ^ Original configuration with which the application was started
  , _rStorage :: ModeStorage mode -- ^ The actual storage
  , _rLogger  :: L.Logger AppName -- ^ The Logger
  }

makeLenses ''Runtime

-- | The command centre: the MTL stack that processes all our computations
newtype AppM (mode :: AppMode) a
  = AppM { runAppM :: ReaderT (Runtime mode) (ExceptT RuntimeErr IO) a
         }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader (Runtime mode)
           , MonadError RuntimeErr
           )

instance L.MonadLog AppName (AppM mode) where
  askLogger = asks _rLogger
  localLogger f = local $ over rLogger f
