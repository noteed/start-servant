{-# LANGUAGE OverloadedStrings #-}
module Prototype.Data.Examples where

import Prototype.Types


--------------------------------------------------------------------------------
users :: [(Text, Profile)] 
users =
  [ ("secret", Profile "alice" "alice@example.com" "Alice")
  ]

todoLists :: [(Text, TodoList)]
todoLists =
  [ ("TL-1", TodoList "start-servant" [TodoItem "Create a test suite" Todo])
  ]

namespaceTodoLists :: [(Text, [Text])]
namespaceTodoLists =
  [ ("alice", ["TL-1"])
  ]
