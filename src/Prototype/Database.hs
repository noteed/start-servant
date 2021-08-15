{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Prototype.Database where

import Data.List (nub, sort)
import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar, STM, TVar)

import qualified Prototype.Data.Examples as Examples
import Prototype.Types


--------------------------------------------------------------------------------

data Handle = Handle
  { hCounter :: TVar Counter
  , hSessions :: TVar [Session]
    -- ^ This is only to keep track of logged in users. It means that in
    -- addition of having a User set in a cookie, the corresponding session
    -- must be present here.
  , hUsers :: TVar [(String, Profile)]
    -- ^ Password, and User. Those are real users. We don't store the password
    -- in a specific data type to avoid manipulating it and risking sending it
    -- over the wire.
  , hTodoLists :: TVar [(String, TodoList)]
    -- ^ Associates namespaces to Todo lists.
  }


--------------------------------------------------------------------------------
newHandle :: IO Handle
newHandle = atomically $ do
  hCounter <- newCounter
  hSessions <- newSessions
  hUsers <- newUsers
  hTodoLists <- newTodoLists
  return Handle {..}

apply :: Handle -> Operation -> STM ()
apply h BumpCounter = bumpCounter h


--------------------------------------------------------------------------------
newCounter = newTVar (Counter 1)

getCounter h = do
  readTVar (hCounter h)

bumpCounter h = do
  Counter i <- readTVar (hCounter h)
  writeTVar (hCounter h) (Counter $ i + 1)


--------------------------------------------------------------------------------
getProfiles :: Handle -> STM [Profile]
getProfiles h = do
  users <- readTVar (hUsers h)
  return (map snd users)

getProfile h namespace_ = do
  users <- readTVar (hUsers h)
  return (lookup' users)
  where

  lookup' :: [(String, Profile)] -> Maybe Profile
  lookup' profiles = case filter f profiles of
    [(_, p)] -> Just p
    _ -> Nothing
  f (_, profile) =
    namespace (profile :: Profile) == namespace_

getProfileAndLists h namespace = do
  mprofile <- getProfile h namespace
  case mprofile of
    Just profile -> do
      lists <- getTodoLists h namespace
      return (Just (profile, lists))
    Nothing -> return Nothing


--------------------------------------------------------------------------------
newTodoLists = newTVar Examples.todoLists

getTodoLists h namespace = do
  lists <- readTVar (hTodoLists h)
  return ((map snd . filter f) lists)
  where f = (namespace ==) . fst


--------------------------------------------------------------------------------
newSessions :: STM (TVar [Session])
newSessions = newTVar []
  -- TODO Each Session should be in its own TVar.

getSessions :: Handle -> STM [Session]
getSessions = readTVar . hSessions

-- In addition of `authenticateProfile`, we can call this function create a
-- session.
addSession h username = do
  sessions <- readTVar (hSessions h)
  writeTVar (hSessions h) (addSession' sessions (Session username))

addSession' :: [Session] -> Session -> [Session]
addSession' ss s = sort (nub (s : ss))


--------------------------------------------------------------------------------
newUsers = newTVar Examples.users
  -- TODO Each Profile should be in its own TVar.

getUsers h = readTVar (hUsers h)


--------------------------------------------------------------------------------
login h credentials = do
  profiles <- readTVar (hUsers h)
  case authenticateProfile credentials profiles of
    Just Profile {..} -> do
      addSession h namespace
      let user = User namespace email
      return (Just user)
    Nothing -> return Nothing

-- Return a Profile matching a User (which comes from a cookie). It also checks
-- that a corresponding session exists.
getLoggedInProfile h user = do
  sessions <- getSessions h
  case lookupSession user sessions of
    Nothing -> return Nothing
    Just _ -> do
      profiles <- getUsers h
      return $ lookupProfile user profiles


--------------------------------------------------------------------------------
-- Convert the submitted login Credentials to a Profile.
authenticateProfile :: Credentials -> [(String, Profile)] -> Maybe Profile
authenticateProfile credentials profiles = case filter f profiles of
  [(_, p)] -> Just p
  _ -> Nothing
  where
  f (pw, profile) =
    namespace (profile :: Profile) == username (credentials :: Credentials) &&
    pw == password (credentials :: Credentials)

-- Convert a User (taken from a signed cookie) to a Profile.
lookupProfile :: User -> [(String, Profile)] -> Maybe Profile
lookupProfile user profiles = case filter f profiles of
  [(_, p)] -> Just p
  _ -> Nothing
  where
  f (_, profile) =
    namespace (profile :: Profile) == username (user :: User)

-- In addition of `lookupProfile`, we can call this function to make sure a
-- session was created.
lookupSession :: User -> [Session] -> Maybe Session
lookupSession user sessions = case filter f sessions of
  [s] -> Just s
  _ -> Nothing
  where
  f session =
    username (session :: Session) == username (user :: User)
