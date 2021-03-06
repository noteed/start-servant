{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype.Runtime.StmDatabase
  ( Handle(..)
  , newHandle
  , apply
  , getCounter
  , bumpCounter
  , getProfiles
  , getProfile
  , newCounter
  , login
  , getLoggedInProfile
  , getProfileAndLists
  , getProfileAndList
  , getSessions
  , getAllTodoLists
  -- * Commonly used updates
  -- ** User
  , namespaceGroupsIO
  , namespaceGroupsSTM
  , addUsersToGroupIO
  , addUsersToGroupSTM
  , createUserIO
  , createUserSTM
  -- ** TodoLists 
  , markItemSTM
  , markItemIO
  , addItemSTM
  , addItemIO
  , deleteItemSTM
  , deleteItemIO
  , editItemSTM
  , editItemIO
  -- * General functions, generating new IDs.
  , newIdIO
  ) where

import qualified Control.Concurrent.STM        as STM
import           Control.Lens                  as Lens
import           Data.List                      ( nub
                                                , sort
                                                )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import qualified Data.Set                      as Set
import qualified ListT
import           ListT                          ( toList )
import qualified Network.HTTP.Types.Status     as Stat
import           Protolude               hiding ( Handle
                                                , toList
                                                )
import           Prototype.ACL                  ( GroupId )
import qualified Prototype.Runtime.Errors      as Errs
import           Prototype.Types.Secret
import qualified StmContainers.Map             as STM
                                                ( Map )
import qualified StmContainers.Map             as STM.Map
import           System.Random                  ( randomIO )

import qualified Prototype.Data.Examples       as Examples
import           Prototype.Types


--------------------------------------------------------------------------------

data Handle = Handle
  { hCounter            :: STM.TVar Counter
  , hSessions           :: STM.TVar [Session]
    -- ^ This is only to keep track of logged in users. It means that in
    -- addition of having a User set in a cookie, the corresponding session
    -- must be present here.
  , hUsers              :: STM.TVar [(Password, Profile)]
  , hUserGroups         :: STM.Map GroupId (Set Namespace) -- ^ Users and their groups. 
    -- ^ Password, and User. Those are real users. We don't store the password
    -- in a specific data type to avoid manipulating it and risking sending it
    -- over the wire.
  , hTodoLists          :: STM.Map TodoListId TodoList
  , hNamespaceTodoLists :: STM.Map Namespace [TodoListId]
    -- ^ Associates namespaces to all their Todo lists.
  }


--------------------------------------------------------------------------------
-- | Generate a new STM based storage.
newHandle :: MonadIO m => m Handle
newHandle = liftIO . atomically $ do
  hCounter            <- newCounter
  hSessions           <- newSessions
  hUsers              <- newUsers
  hUserGroups         <- newUserGroupsSTM
  hTodoLists          <- newTodoLists
  hNamespaceTodoLists <- newNamespaceTodoLists
  return Handle { .. }

apply :: Handle -> Operation -> STM ()
apply h BumpCounter = bumpCounter h

-- | Get the groups a particular namespace (user) is associated with.
namespaceGroupsIO :: MonadIO m => Handle -> Namespace -> m (Set GroupId)
namespaceGroupsIO h = liftIO . atomically . namespaceGroupsSTM h

-- | Get the groups a particular namespace (user) is associated with.
namespaceGroupsSTM :: Handle -> Namespace -> STM (Set GroupId)
namespaceGroupsSTM Handle { hUserGroups = STM.Map.listT -> hUserGroups } ns =
  ListT.fold collectGroupsBelonging mempty hUserGroups
 where
  collectGroupsBelonging acc (gid, users) =
    pure $ if ns `elem` users then acc `Set.union` (Set.singleton gid) else acc

--------------------------------------------------------------------------------
newCounter = STM.newTVar (Counter 1)

getCounter h = STM.readTVar (hCounter h)

bumpCounter h = do
  Counter i <- STM.readTVar (hCounter h)
  STM.writeTVar (hCounter h) (Counter $ i + 1)

--------------------------------------------------------------------------------
getProfiles :: Handle -> STM [Profile]
getProfiles h = do
  users <- STM.readTVar (hUsers h)
  return (map snd users)

getProfile h namespace_ = do
  users <- STM.readTVar (hUsers h)
  return (lookup' users)
 where

  lookup' profiles = case filter f profiles of
    [(_, p)] -> Just p
    _        -> Nothing
  f (_, profile) = namespace (profile :: Profile) == namespace_

getProfileAndLists :: Handle -> Namespace -> STM (Maybe (Profile, [TodoList]))
getProfileAndLists h namespace = do
  mprofile <- getProfile h namespace
  case mprofile of
    Just profile -> do
      lists <- getTodoLists h namespace
      return (Just (profile, lists))
    Nothing -> return Nothing

getProfileAndList
  :: Handle -> Namespace -> Text -> STM (Maybe (Profile, TodoList))
getProfileAndList h namespace listname = do
  mprofile <- getProfile h namespace
  case mprofile of
    Just profile -> do
      mlist <- getTodoList h namespace listname
      case mlist of
        Just list' -> return (Just (profile, list'))
        Nothing    -> return Nothing
    Nothing -> return Nothing

--------------------------------------------------------------------------------
-- | Create an STM map of TODOlists.
newTodoLists :: STM (STM.Map TodoListId TodoList)
newTodoLists = do
  m <- STM.Map.new
  mapM_ (`insertList` m) Examples.allTodoLists
  return m
  where insertList tl@TodoList {..} = STM.Map.insert tl _tlId

getAllTodoLists :: Handle -> STM [(TodoListId, TodoList)]
getAllTodoLists = toList . STM.Map.listT . hTodoLists

getTodoLists :: Handle -> Namespace -> STM [TodoList]
getTodoLists h namespace = do
  mids <- STM.Map.lookup namespace (hNamespaceTodoLists h)
  case mids of
    Nothing  -> return []
    Just ids -> do
      mls <- mapM (`STM.Map.lookup` lists) ids
      return (catMaybes mls)
  where lists = hTodoLists h

getTodoList :: Handle -> Namespace -> Text -> STM (Maybe TodoList)
getTodoList h namespace listname = do
  lists <- getTodoLists h namespace
  pure $ case filter ((listname ==) . _tlName) lists of
    [list'] -> Just list'
    _       -> Nothing

newNamespaceTodoLists = do
  m <- STM.Map.new
  mapM_ (\(k, v) -> STM.Map.insert v k m) Examples.todoListPermissions
  return m


--------------------------------------------------------------------------------
newSessions :: STM (STM.TVar [Session])
newSessions = STM.newTVar []
  -- TODO Each Session should be in its own TVar.

getSessions :: Handle -> STM [Session]
getSessions = STM.readTVar . hSessions

-- In addition of `authenticateProfile`, we can call this function create a
-- session.
addSession h (username :: Namespace) = do
  sessions <- STM.readTVar (hSessions h)
  STM.writeTVar (hSessions h) (addSession' sessions (Session username))

addSession' :: [Session] -> Session -> [Session]
addSession' ss s = sort (nub (s : ss))


--------------------------------------------------------------------------------
newUsers = STM.newTVar Examples.users

newUserGroupsSTM :: STM (STM.Map GroupId (Set Namespace))
newUserGroupsSTM = do
  m <- STM.Map.new
  mapM_ (uncurry $ addUsersToGroupSTM m) userGroupsL
  pure m
  where userGroupsL = Map.toList Examples.userGroups

addUsersToGroupSTM
  :: STM.Map GroupId (Set Namespace) -> GroupId -> Set Namespace -> STM ()
addUsersToGroupSTM m gid users = do
  mExistingUsers <- STM.Map.lookup gid m
  let newUsers' = maybe users (Set.union users) mExistingUsers
  STM.Map.insert newUsers' gid m

addUsersToGroupIO
  :: MonadIO m
  => STM.Map GroupId (Set Namespace)
  -> GroupId
  -> Set Namespace
  -> m ()
addUsersToGroupIO m gid = liftIO . atomically . addUsersToGroupSTM m gid

-- | Create a new user 
createUserSTM
  :: Profile
  -> Password
  -> STM.TVar [(Password, Profile)]
  -> STM (Maybe StmStorageErr)
createUserSTM p pwd profPwds = do
  -- First, get all existing users to identify namespace collissions. 
  profs <- fmap snd <$> STM.readTVar profPwds
  let coll = find ((== newNs) . namespace) profs
  if isJust coll
    then pure (Just . NamespaceCollission $ newNs)
    else STM.modifyTVar' profPwds ((pwd, p) :) $> Nothing
  where newNs = namespace p

-- | Create a new  user 
createUserIO
  :: MonadIO m
  => Profile
  -> Password
  -> STM.TVar [(Password, Profile)]
  -> m (Maybe StmStorageErr)
createUserIO p pwd = liftIO . atomically . createUserSTM p pwd

  -- TODO Each Profile should be in its own TVar.

getUsers h = STM.readTVar (hUsers h)

--------------------------------------------------------------------------------
login h credentials = do
  profiles <- STM.readTVar (hUsers h)
  case authenticateProfile credentials profiles of
    Just Profile {..} -> do
      addSession h namespace
      let user = User namespace email profTagRels
      return (Just user)
    Nothing -> return Nothing

-- Return a Profile matching a User (which comes from a cookie). It also checks
-- that a corresponding session exists.
getLoggedInProfile h user = do
  sessions <- getSessions h
  case lookupSession user sessions of
    Nothing -> return Nothing
    Just _  -> do
      profiles <- getUsers h
      return $ lookupProfile user profiles

--------------------------------------------------------------------------------
-- | Convert the submitted login Credentials to a Profile.
authenticateProfile
  :: Credentials -> [(Secret '[] Text, Profile)] -> Maybe Profile
authenticateProfile credentials profiles = case filter f profiles of
  [(_, p)] -> Just p
  _        -> Nothing
 where
  f (pw, profile) =
    namespace (profile :: Profile)
      ==  username (credentials :: Credentials)
      &&  pw
      =:= password (credentials :: Credentials)

-- Convert a User (taken from a signed cookie) to a Profile.
lookupProfile :: User -> [(Secret '[] Text, Profile)] -> Maybe Profile
lookupProfile user profiles = case filter f profiles of
  [(_, p)] -> Just p
  _        -> Nothing
 where
  f (_, profile) = namespace (profile :: Profile) == username (user :: User)

-- In addition of `lookupProfile`, we can call this function to make sure a
-- session was created.
lookupSession :: User -> [Session] -> Maybe Session
lookupSession user sessions = case filter f sessions of
  [s] -> Just s
  _   -> Nothing
  where f session = username (session :: Session) == username (user :: User)

data StmStorageErr where
  NamespaceCollission ::Namespace -> StmStorageErr
  RelatedErr ::(Errs.IsRuntimeErr err, Show err) => err -> StmStorageErr

deriving instance Show StmStorageErr

instance Errs.IsRuntimeErr StmStorageErr where
  errCode = \case
    NamespaceCollission{} -> specificCode "NAMESPACE_EXISTS"
    RelatedErr re'        -> Errs.errCode re'
    where specificCode = mappend "ERR.STM_STORAGE"
  httpStatus = \case
    NamespaceCollission{} -> Stat.conflict409
    RelatedErr re'        -> Errs.httpStatus re'
  userMessage = \case
    NamespaceCollission ns  -> Just $ "Namespace taken: " <> show ns
    RelatedErr          re' -> Errs.userMessage re'

-- | Perform an operation with a list, if found.
withTodoListSTM
  :: TodoListId
  -> STM.Map TodoListId TodoList
  -> (TodoList -> STM (Maybe StmStorageErr))
  -> STM (Maybe StmStorageErr)
withTodoListSTM lid lists withList =
  STM.Map.lookup lid lists >>= maybe noList withList
  where noList = pure . Just . RelatedErr $ NoSuchTodoList lid

-- | Mark a todolist item with a new `TodoState`
markItemSTM
  :: TodoListId
  -> TodoItemId
  -> TodoState
  -> STM.Map TodoListId TodoList
  -> STM (Maybe StmStorageErr)
markItemSTM lid iid state' lists = withTodoListSTM lid lists markInList
 where
   -- Traverse the items of the list and set the state.
   -- We use the extremely powerful traversals that let us target the matching items & modify them in-place.
  markInList oldList =
    let updatedList =
          oldList & tlItems %~ traverse . filtered hasId %~ set tiState state'
    -- Finally, we'd like to see if we did affect some items; if no items were affected
    -- we can assume that the item already had the given state, or was not present.
    in  if oldList /= updatedList
          then STM.Map.insert updatedList lid lists $> Nothing
          else noItem
  hasId  = (iid ==) . _tiId
  noItem = pure . Just . RelatedErr $ NoSuchItem lid iid

markItemIO
  :: MonadIO m
  => TodoListId
  -> TodoItemId
  -> TodoState
  -> STM.Map TodoListId TodoList
  -> m (Maybe StmStorageErr)
markItemIO lid iid state' = liftIO . atomically . markItemSTM lid iid state'

-- | Add a new item to a todo-list, if it exists.
addItemSTM
  :: TodoListId
  -> TodoItem
  -> STM.Map TodoListId TodoList
  -> STM (Maybe StmStorageErr)
addItemSTM lid item lists = withTodoListSTM lid lists insertItem
 where
  insertItem list' =
    let mCollission = headMay $ list' ^. tlItems ^.. folded . filtered sameId
    in  case mCollission of
          Nothing ->
            let newList = list' & tlItems %~ (item :)
            in  STM.Map.insert newList lid lists $> Nothing
          Just _ -> pure . Just . RelatedErr $ ItemIdCollission lid newItemId
  newItemId = item ^. tiId
  sameId    = (== newItemId) . _tiId

addItemIO
  :: MonadIO m
  => TodoListId
  -> TodoItem
  -> STM.Map TodoListId TodoList
  -> m (Maybe StmStorageErr)
addItemIO lid item = liftIO . atomically . addItemSTM lid item

-- | Delete an item from a todo-list
deleteItemSTM
  :: TodoListId
  -> TodoItemId
  -> STM.Map TodoListId TodoList
  -> STM (Maybe StmStorageErr)
deleteItemSTM lid iid lists = withTodoListSTM lid lists delItem
 where
  delItem oldList =
    let updatedList = oldList & tlItems %~ filter (not . (== iid) . _tiId)
    in  if oldList /= updatedList
          then STM.Map.insert updatedList lid lists $> Nothing
          else noItem

  noItem = pure . Just . RelatedErr $ NoSuchItem lid iid

deleteItemIO
  :: MonadIO m
  => TodoListId
  -> TodoItemId
  -> STM.Map TodoListId TodoList
  -> m (Maybe StmStorageErr)
deleteItemIO lid iid = liftIO . atomically . deleteItemSTM lid iid

editItemSTM
  :: TodoListId
  -> TodoItem
  -> STM.Map TodoListId TodoList
  -> STM (Maybe StmStorageErr)
editItemSTM lid newItem lists = withTodoListSTM lid lists editItem
 where
  editItem oldList =
    let updatedList = oldList & tlItems %~ replaceNew
    in  STM.Map.insert updatedList lid lists $> Nothing
  replaceNew items =
    [ item
    | oldItem <- items
    , let item = if oldItem ^. tiId == itemId then newItem else oldItem
    ]
  itemId = newItem ^. tiId

editItemIO
  :: MonadIO m
  => TodoListId
  -> TodoItem
  -> STM.Map TodoListId TodoList
  -> m (Maybe StmStorageErr)
editItemIO lid newItem = liftIO . atomically . editItemSTM lid newItem

-- | Generate an ID with a prefix.
newIdIO :: MonadIO m => Text -> m Text
newIdIO prefix =
  liftIO $ mappend (prefix <> "-") . show . abs <$> randomIO @Int
