{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Prototype.Data.Examples
  ( users
  , allTodoLists
  , todoListPermissions
  ) where

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
                    [groupBackendEng]
                    [(TagOwn, [tagEng]), (TagRead, [tagAccounting])]


-- * Tags

tagEng = Tag "engineering"
tagAccounting = Tag "accounting"


-- * Resources

-- ** TodoLists

-- | TodoList's Alice has permissions on 
aliceTodoLists :: [TodoList]
aliceTodoLists =
  [ TodoList "TL-1"
             "start-servant"
             [TodoItem "Create a test suite" Todo]
             [tagEng, tagAccounting]
  ]

-- | All todolists 
allTodoLists :: [TodoList]
allTodoLists = aliceTodoLists

-- | User resources: todolists that the users have permissions on. 
todoListPermissions :: [(Namespace, [TodoListId])]
todoListPermissions = [(nsAlice, tlId <$> aliceTodoLists)]
