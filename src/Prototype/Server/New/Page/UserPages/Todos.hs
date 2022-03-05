{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages.Todos
  (-- * Todos 
    TodoListSummary(..)
  , UserTodos(..)
  , TodoListCreatePage(..)
  -- * Re-exports
  , module Item
  ) where

import qualified Data.Text                     as T
import           Protolude
import qualified Prototype.Server.New.Page.Shared
                                               as Shared
import           Prototype.Server.New.Page.UserPages.Todos.Item
                                               as Item
import qualified Prototype.Types               as Types
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

newtype TodoListSummary = TodoListSummary Types.TodoList

instance H.ToMarkup TodoListSummary where
  toMarkup (TodoListSummary Types.TodoList {..}) = H.a (H.toMarkup @Text text)
    ! A.href link'
   where
    numItems = show $ length _tlItems
    text =
      T.unwords ["Name:", _tlName, "with", "(" <> numItems <> ")", "items."]
    link' = H.textValue $ Item.userTodoPath _tlId

newtype UserTodos = UserTodos [TodoListSummary]

instance H.ToMarkup UserTodos where
  toMarkup (UserTodos summaries) = do
    Shared.titledList "Your todo-lists" summaries
    H.a "Add a new list" ! A.href "/private/user/todos/new-list/form"

-- | A page for creating a todolist.
data TodoListCreatePage = TodoListCreatePage

instance H.ToMarkup TodoListCreatePage where

  toMarkup _ = H.form inputs ! A.action link' ! A.method "POST"
   where
    inputs = do
      "Name: " >> H.input ! A.name "_tlName" ! A.type_ "text"
      "Tag: " >> H.input ! A.name "_tlTags" ! A.type_ "text"
      H.button "Create" ! A.type_ "submit" ! A.action link' ! A.method "POST"
    link' = "/private/user/todos/new-list/create"
