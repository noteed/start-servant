{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages.Todos
  (-- * Todos 
    TodoListSummary(..)
  , UserTodos(..)
  -- * Views specific to permissions
  , RWView(..)
  , ROView(..)
  -- * Re-exports
  , module Item
  ) where

import           Control.Lens                  as Lens
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Prototype.Server.New.Page.Shared
                                               as Shared
import           Prototype.Server.New.Page.UserPages.Todos.Item
                                               as Item
import qualified Prototype.Types               as Types
import           Prototype.Types.NonEmptyText
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
    link' = H.toValue $ "/private/user/todos/" <> _tlId

newtype UserTodos = UserTodos [TodoListSummary]

instance H.ToMarkup UserTodos where
  toMarkup (UserTodos summaries) =
    Shared.titledList "Your todo-lists" summaries

