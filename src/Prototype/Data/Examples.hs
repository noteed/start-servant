{-# LANGUAGE OverloadedStrings #-}
module Prototype.Data.Examples
  ( users
  , todoLists
  , namespaceTodoLists
  ) where

import           Prototype.Types


--------------------------------------------------------------------------------
users :: [(Text, Profile)]
users = [("secret", Profile "alice" "alice@example.com" "Alice")]

todoLists :: [(Text, TodoList)]
todoLists =
  [("TL-1", TodoList "start-servant" [TodoItem "Create a test suite" Todo])]

namespaceTodoLists :: [(Namespace, [TodoListId])]
namespaceTodoLists = [("alice", ["TL-1"])]
