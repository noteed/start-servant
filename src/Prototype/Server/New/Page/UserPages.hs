{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages
  ( -- * Groups
    UserGroups(..)
  -- * Todos 
  , TodoListSummary(..)
  , UserTodos(..)
  ) where

import qualified Data.Text                     as T
import qualified Prototype.ACL                 as ACL
import qualified Prototype.Server.New.Page.Shared
                                               as Shared
import qualified Prototype.Types               as Types
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

newtype UserGroups = UserGroups (Set ACL.GroupId)

instance H.ToMarkup UserGroups where
  toMarkup (UserGroups ids) = Shared.titledList "Your groups" ids

newtype TodoListSummary = TodoListSummary Types.TodoList

instance H.ToMarkup TodoListSummary where
  toMarkup (TodoListSummary Types.TodoList {..}) = H.a (H.toMarkup @Text text)
    H.! A.href link'
   where
    numItems = show $ length tlItems
    text =
      T.unwords ["Name:", tlName, "with", "(" <> numItems <> ")", "items."]
    link' = H.toValue $ "/private/user/todos/" <> tlId

newtype UserTodos = UserTodos [TodoListSummary]

instance H.ToMarkup UserTodos where
  toMarkup (UserTodos summaries) =
    Shared.titledList "Your todo-lists" summaries
