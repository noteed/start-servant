{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Prototype.Data.Examples
  ( users
  , userGroups
  , groupBackendEng
  , allTodoLists
  , todoListPermissions
  ) where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Prototype.ACL
import           Prototype.Types
import           Prototype.Types.Secret


--------------------------------------------------------------------------------

-- * Groups

groupBackendEng = GroupId "backend-eng-group"

-- * Users/Profiles

-- ** Namespaces

nsAlice = Namespace "alice"

users :: [(Secret '[] Text, Profile)]
users = [("secret", profAlice)]

profAlice = Profile nsAlice
                    "alice@example.com"
                    "Alice"
                    [(TagOwn, [tagEng]), (TagRead, [tagAccounting])]


-- ** Users and their groups

userGroups = Map.fromList [(groupBackendEng, Set.singleton nsAlice)]

-- * Tags

tagEng = Tag "engineering"
tagAccounting = Tag "accounting"
tagHR = Tag "HR"


-- * Resources

-- ** TodoLists

-- | TodoList's Alice has permissions on 
aliceTodoLists :: [TodoList]
aliceTodoLists =
  [ TodoList
    "TL-1"
    "start-servant"
    [ TodoItem "Create a test suite"       Todo
    , TodoItem "Have some fun programming" Done
    ]
    [tagEng, tagAccounting]
  , TodoList
    "TL-0"
    "Alice read-only"
    [ TodoItem "Let Alice read this"     Todo
    , TodoItem "Let Alice read this too" Done
    ]
    [tagAccounting]
  , TodoList "TL-HR0"
             "HR dept. only"
             [TodoItem "Only HR dept. can see this" Todo]
             [tagHR]
  ]

-- | All todolists 
allTodoLists :: [TodoList]
allTodoLists = aliceTodoLists

-- | User resources: todolists that the users have permissions on. 
todoListPermissions :: [(Namespace, [TodoListId])]
todoListPermissions = [(nsAlice, tlId <$> aliceTodoLists)]
