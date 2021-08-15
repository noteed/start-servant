module Prototype.Data.Examples where

import Prototype.Types


--------------------------------------------------------------------------------
users =
  [ ("secret", Profile "alice" "alice@example.com" "Alice")
  ]

todoLists =
  [ ("alice", TodoList "start-servant" [TodoItem "Create a test suite" Todo])
  ]
