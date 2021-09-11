{-# LANGUAGE RankNTypes #-}
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
  , cServerPort
  , cCookieSettings
  , cMkJwtSettings
  , cServerMode
  , AppMode(..)
  , ServerMode(..)
  , AppName(..)
  , showAppName
  , unAppName
  , ModeStorage
  -- * Runtime
  , Runtime(..)
  , StmRuntime
  , PostgresRuntime
  , rConf
  , rStorage
  , rLogger
  -- * Control
  , AppM(..)
  , PostgresAppM
  , StmAppM
  -- * Booting in specific modes.
  -- TODO: add postgres mode in the future.
  , bootStm
  ) where

import           Control.Lens
import           Control.Monad.Log             as L
import qualified Crypto.JOSE.JWK               as JWK
import qualified Data.String                   as Str
import qualified Data.Text                     as T
-- Needed for handwritten Show instance for Conf
import qualified GHC.Show
import           Prelude                 hiding ( Handle )
import           Prototype.Runtime.Errors
import qualified Prototype.Runtime.StmDatabase as Db
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Types               as Ptypes
import qualified Servant.Auth.Server           as Srv

-- | An application name: lets us group logging etc. with @/@ as separators.
newtype AppName = AppName { _unAppName :: [Text] }
                deriving (Eq, Show, Semigroup, Monoid) via [Text]

instance TextShow AppName where
  showb = showb . showAppName

-- | Reverse of the IsString instance (below)
showAppName (AppName envs) = T.intercalate "/" envs

makeLenses ''AppName

-- | Take any string; split at @/@; and use it as the AppName.
instance IsString AppName where
  fromString = AppName . T.splitOn "/" . T.pack

{- | Server modes

== Legacy
Starts with using the existing implementation at the time of writing: here all operations run in IO

== New
Starts using the `AppM` stacks; in a given `Stm` or `Postgres` mode.
-}
data ServerMode = Legacy | New
                deriving (Eq, Show, Read)

instance Default ServerMode where
  def = Legacy

data Conf = Conf
  { _cAppName        :: AppName -- ^ Application name
  , _cLogLevel       :: L.Level -- ^ The logging level
  , _cServerPort     :: Int -- ^ The port number to run the server on
  , _cCookieSettings :: Srv.CookieSettings -- ^ Cookie settings to use
  , _cMkJwtSettings  :: JWK.JWK -> Srv.JWTSettings -- ^ JWK settings to use.
  , _cServerMode     :: ServerMode  -- ^ Server mode to start in
  }

makeLenses ''Conf

instance Show Conf where
  show Conf {..} =
    T.unpack
      . T.intercalate "\n\t"
      $ [ "\tappName = " <> showAppName _cAppName
        , "logLevel = " <> show _cLogLevel
        , "serverPort = " <> show _cServerPort
        , "cookieSettings = " <> show _cCookieSettings
        , "serverMode = " <> show _cServerMode
        ]

instance Default Conf where
  def = Conf
    { _cAppName        = "start-servant"
    , _cLogLevel       = L.levelInfo 
    , _cServerPort     = 7249
    -- Disable XSRF Cookie (otherwise, this needs some logic instead of
    -- simple cURL calls):
    -- https://github.com/haskell-servant/servant-auth/issues/55#issuecomment-747046527
    -- Using Secure with Same-Site=Strict would be good enough ?
    , _cCookieSettings = Srv.defaultCookieSettings
                           { Srv.cookieIsSecure    = Srv.NotSecure
            -- ^ Use temporarily NotSecure for easier local testing with cURL.
                           , Srv.cookieXsrfSetting = Nothing
                           , Srv.cookieSameSite    = Srv.SameSiteStrict
                           }
    , _cMkJwtSettings  = Srv.defaultJWTSettings
    , _cServerMode     = def
    }

data AppMode = Stm | Postgres
             deriving (Eq, Show)

-- | A Type level function that gives us the kind of storage
-- based on the mode of the application.
type family ModeStorage (mode :: AppMode) :: Type where
  ModeStorage 'Stm = Db.Handle
  -- Later: added as an example
  -- ModeStorage 'Postres = ConnectionPool

-- | STM based runtime
data Runtime (mode :: AppMode) = Runtime
  { _rConf    :: Conf -- ^ Original configuration with which the application was started
  , _rStorage :: ModeStorage mode -- ^ The actual storage
  , _rLogger  :: L.Logger AppName -- ^ The Logger
  }

makeLenses ''Runtime

-- | Type synonym for convenience
type StmRuntime = Runtime 'Stm
type PostgresRuntime = Runtime 'Postgres

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

type StmAppM = AppM 'Stm
type PostgresAppM = AppM 'Postgres

instance L.MonadLog AppName (AppM mode) where
  askLogger = asks _rLogger
  localLogger f = local $ over rLogger f

-- | Boot in the Stm storage mode.
bootStm :: MonadIO m => Conf -> m (Either RuntimeErr StmRuntime)
bootStm _rConf@Conf {..} = do
  -- Start off by creating a logger first; we may want to log steps in the boot process.
  _rLogger <- createLogger
  L.runLogT' _rLogger . bootEnv $ do
    -- Instantiate storage
    L.info "Instantiating storage"
    _rStorage <- liftIO Db.newHandle
    L.info "Booted!" $> Right Runtime { .. }
 where
  bootEnv      = L.localEnv (<> "Boot" <> "STM")
  createLogger = L.makeDefaultLogger L.simpleTimeFormat
                                     (L.LogStdout 1024)
                                     _cLogLevel
                                     _cAppName

-- | Given we're operating in an Stm based environment, how do we carry out user operations?
instance S.DBStorage StmAppM Ptypes.User where

  -- All of these are stubs that should be implemented.
  dbUpdate up = withStorage $ case up of
    CreateNewUser  u     -> undefined
    DeactivateUser uid   -> undefined
    AddToGroups uid gids -> undefined

  dbSelect sel = withStorage $ case sel of
    AuthUser creds -> liftIO . fmap toList . atomically . (`Db.login` creds)

-- | Get the storage handle, and give it to the function that does something with it.
withStorage :: forall a mode . (ModeStorage mode -> AppM mode a) -> AppM mode a
withStorage f = asks _rStorage >>= f
