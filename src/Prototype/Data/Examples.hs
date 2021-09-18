{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Prototype.Data.Examples
  ( users
  , todoLists
  , namespaceTodoLists
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

todoLists :: [(TodoListId, TodoList)]
todoLists =
  [ ( "TL-1"
    , TodoList "start-servant"
               [TodoItem "Create a test suite" Todo]
               [tagEng, tagAccounting]
    )
  ]

namespaceTodoLists :: [(Namespace, [TodoListId])]
namespaceTodoLists = [(nsAlice, ["TL-1"])]
