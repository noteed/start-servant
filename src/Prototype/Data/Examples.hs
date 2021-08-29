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

groupBackendEng = GroupId "backend-eng-group"

users :: [(Secret '[] Text, Profile)]
users = [("secret", profAlice)]

profAlice = Profile "alice"
                    "alice@example.com"
                    "Alice"
                    [groupBackendEng]
                    [(TagOwn, [tagEng]), (TagRead, [tagAccounting])]

tagEng = Tag "engineering"
tagAccounting = Tag "accounting"

todoLists :: [(Text, TodoList)]
todoLists =
  [ ( "TL-1"
    , TodoList "start-servant"
               [TodoItem "Create a test suite" Todo]
               [tagEng, tagAccounting]
    )
  ]

namespaceTodoLists :: [(Namespace, [TodoListId])]
namespaceTodoLists = [("alice", ["TL-1"])]
