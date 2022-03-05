{-# LANGUAGE ViewPatterns #-}
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
  , cStaticFilesDir
  , AppMode(..)
  , ServerMode(..)
  , Logging.AppName(..)
  , Logging.showAppName
  , Logging.unAppName
  , ModeStorage
  -- * Runtime
  , Runtime(..)
  , StmRuntime
  , PostgresRuntime
  , rConf
  , rStorage
  , rLogger
  , rJwtSettings
  -- * Control
  , AppM(..)
  , PostgresAppM
  , StmAppM
  -- ** Running
  , appMHandlerNatTrans
  -- * Booting in specific modes.
  -- TODO: add postgres mode in the future.
  , bootStm
  ) where

import qualified Control.Concurrent.STM        as STM
import           Control.Lens            hiding ( Level )
import qualified Crypto.JOSE.JWK               as JWK
import           Data.Default.Class             ( Default(..) )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
-- Needed for handwritten Show instance for Conf
import qualified GHC.Show
import qualified Logging
import           Protolude               hiding ( Handle )
import qualified Prototype.ACL                 as ACL
import           Prototype.Runtime.Errors
import qualified Prototype.Runtime.StmDatabase as Db
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Types               as Ptypes
import           Prototype.Types.NonEmptyText   ( nonEmptyText )
import qualified Servant.Auth.Server           as Srv
import           Servant.Server                 ( Handler(..) )

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
  { _cAppName        :: Logging.AppName -- ^ Application name
  , _cLogLevel       :: Logging.Level -- ^ The logging level
  , _cServerPort     :: Int -- ^ The port number to run the server on
  , _cCookieSettings :: Srv.CookieSettings -- ^ Cookie settings to use
  , _cMkJwtSettings  :: JWK.JWK -> Srv.JWTSettings -- ^ JWK settings to use.
  , _cServerMode     :: ServerMode  -- ^ Server mode to start in
  , _cStaticFilesDir :: FilePath
  }

makeLenses ''Conf

instance Show Conf where
  show Conf {..} =
    T.unpack
      . T.intercalate "\n\t"
      $ [ "\tappName = " <> Logging.showAppName _cAppName
        , "logLevel = " <> show _cLogLevel
        , "serverPort = " <> show _cServerPort
        , "cookieSettings = " <> show _cCookieSettings
        , "serverMode = " <> show _cServerMode
        ]

instance Default Conf where
  def = Conf
    { _cAppName        = "start-servant"
    , _cLogLevel       = Logging.levelInfo
    , _cServerPort     = 7249
    -- Disable XSRF Cookie (otherwise, this needs some logic instead of
    -- simple cURL calls):
    -- https://github.com/haskell-servant/servant-auth/issues/55#issuecomment-747046527
    -- Using Secure with Same-Site=Strict would be good enough ?
    , _cCookieSettings = Srv.defaultCookieSettings
                           { Srv.cookieIsSecure    = Srv.NotSecure -- Use temporarily NotSecure for easier local testing with cURL.
                           , Srv.cookieXsrfSetting = Nothing
                           , Srv.cookieSameSite    = Srv.SameSiteStrict
                           }
    , _cMkJwtSettings  = Srv.defaultJWTSettings
    , _cServerMode     = def
    , _cStaticFilesDir = "/static"
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
  { _rConf        :: Conf -- ^ Original configuration with which the application was started
  , _rStorage     :: ModeStorage mode -- ^ The actual storage
  , _rLogger      :: Logging.Logger Logging.AppName -- ^ The Logger
  , _rJwtSettings :: Srv.JWTSettings  -- ^ JWT settings to use
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

instance Logging.MonadLog Logging.AppName (AppM mode) where
  askLogger = asks _rLogger
  localLogger f = local $ over rLogger f

-- | Boot in the Stm storage mode.
bootStm :: MonadIO m => JWK.JWK -> Conf -> m (Either RuntimeErr StmRuntime)
bootStm jwk _rConf@Conf {..} = do
  -- Start off by creating a logger first; we may want to log steps in the boot process.
  _rLogger <- createLogger
  Logging.runLogT' _rLogger . bootEnv $ do
    -- Instantiate storage
    Logging.infoE "Instantiating storage"
    _rStorage <- liftIO Db.newHandle
    Logging.info "Booted"
      $> Right Runtime { _rJwtSettings = _cMkJwtSettings jwk, .. }
 where
  bootEnv      = Logging.localEnv (<> "Boot" <> "STM")
  createLogger = Logging.makeDefaultLogger Logging.simpleTimeFormat
                                           (Logging.LogStdout 1024)
                                           _cLogLevel
                                           _cAppName

-- | Get the storage handle, and give it to the function that does something with it.
withStorage :: forall a mode . (ModeStorage mode -> AppM mode a) -> AppM mode a
withStorage f = asks _rStorage >>= f

-- | Natural transformation from some `AppM` in any given mode, to a servant Handler. 
appMHandlerNatTrans :: forall mode a . Runtime mode -> AppM mode a -> Handler a
appMHandlerNatTrans rt appM =
  let
    -- We peel off the AppM + ReaderT layers, exposing our ExceptT RuntimeErr IO a
    -- This is very similar to Servant's Handler: https://hackage.haskell.org/package/servant-server-0.17/docs/Servant-Server-Internal-Handler.html#t:Handler
      unwrapReaderT          = (`runReaderT` rt) . runAppM $ appM
      -- Map our errors to `ServantError` 
      runtimeErrToServantErr = withExceptT asServantError
  in 
    -- re-wrap as servant `Handler`
      Handler $ runtimeErrToServantErr unwrapReaderT

-- * Identifying groups of grantees.

instance ACL.GroupedGrantee StmAppM Ptypes.User where
  granteeGroups u =
    withStorage (`Db.namespaceGroupsIO` (u ^. Ptypes.uUsername))

instance ACL.GroupedGrantee StmAppM Ptypes.Profile where
  granteeGroups p = withStorage (`Db.namespaceGroupsIO` Ptypes.namespace p)

-- * DBStorage instances. 

-- | Given we're operating in an Stm based environment, how do we carry out user operations?
instance S.DBStorage StmAppM Ptypes.User where

  -- All of these are stubs that should be implemented.
  dbUpdate = withStorage . \case
    CreateNewUser p pwd -> insertNewUser . Db.hUsers
     where
      insertNewUser tvar =
        Db.createUserIO p pwd tvar >>= maybe reportId throwError'
      reportId = pure [Ptypes.namespace p]

    AddToGroups uid (toList -> gids) -> addToGroups
     where
      addToGroups Db.Handle {..} = mapM addToGroup gids
       where
        addToGroup gid =
          uid <$ Db.addUsersToGroupIO hUserGroups gid (Set.singleton uid)

  dbSelect sel = withStorage $ case sel of
    AuthUser creds -> liftIO . fmap toList . atomically . (`Db.login` creds)

-- | Storage operations for todolists 
instance S.DBStorage StmAppM Ptypes.TodoList where

  dbUpdate = withStorage . \case
    MarkItem lid iid state' ->
      Db.markItemIO lid iid state'
        .   Db.hTodoLists
        >=> maybe (pure [lid]) throwError'
    AddItem lid itemCreate -> \db -> do
      newId <- generateId
      let item = itemCreate & tiId .~ newId
      Db.addItemIO lid item (Db.hTodoLists db)
        >>= maybe (pure [lid]) throwError'
     where
      generateId =
        Db.newIdIO "item" >>= maybe err (pure . TodoItemId) . nonEmptyText
      err = throwError' . IdGenFailed $ "ID generated was empty!"

    DeleteItem lid iid ->
      Db.deleteItemIO lid iid . Db.hTodoLists >=> maybe (pure [lid]) throwError'

    EditItem lid item ->
      Db.editItemIO lid item . Db.hTodoLists >=> maybe (pure [lid]) throwError'

    NewList listCreate -> undefined

  dbSelect = \case
    AllTodoLists ->
      withStorage
        $ liftIO
        . fmap (fmap snd)
        . STM.atomically
        . Db.getAllTodoLists
    TodoListsByNamespace userNamespace ->
      -- first look up the profile, so we can read the tags from the profile. 
      S.dbSelect (LookupProfile userNamespace)
        >>= maybe notFound usingProfileTags
        .   headMay
     where
      usingProfileTags prof = do
        allTls <- S.dbSelect Ptypes.AllTodoLists
        -- Todolists grouped by the user's tag-rels.
        let ACL.GroupedResources groupedTls = ACL.groupResources prof allTls
            userTls = toList $ Map.foldl' Set.union mempty groupedTls
        pure userTls
      notFound = throwError' $ Ptypes.NoSuchUser userNamespace

    -- Naive solution here, but we're not bothered too much for performance in the STM case.
    TodoListById lid ->
      S.dbSelect AllTodoLists <&> toList . find ((== lid) . S.dbId)

-- | Storage operations for profiles.
instance S.DBStorage StmAppM Ptypes.Profile where
  dbSelect = \case
    LookupProfile pNamespace ->
      withStorage $ readProfiles >=> pure . lookupNamespace
     where
      readProfiles Db.Handle {..} = fmap snd <$> liftIO (STM.readTVarIO hUsers)
      lookupNamespace profs =
        [ p | p <- profs, Ptypes.namespace p == pNamespace ]
