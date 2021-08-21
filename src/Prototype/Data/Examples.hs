module Prototype.Data.Examples where

import Prototype.Types


--------------------------------------------------------------------------------
users =
  [ ("secret", Profile "alice" "alice@example.com" "Alice")
  ]

todoLists =
  [ ("TL-1", TodoList "start-servant" [TodoItem "Create a test suite" Todo])
  ]

namespaceTodoLists =
  [ ("alice", ["TL-1"])
  ]
