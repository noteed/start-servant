{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages
  ( -- * Groups
    UserGroups(..)
  -- * Todos 
  , TodoListSummary(..)
  , UserTodos(..)
  -- * Views specific to permissions
  , RWView(..)
  , ROView(..)
  -- ** Specific to Todos
  , TodoListRW
  , TodoListRO
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

newtype RWView resource = RWView resource

type TodoListRW = RWView Types.TodoList

instance H.ToMarkup TodoListRW where
  toMarkup (RWView tl) = todoListInvariantMarkup tl
    $ Shared.titledList "Items" (RWView <$> Types.tlItems tl)

instance H.ToMarkup (RWView Types.TodoItem) where
  toMarkup (RWView Types.TodoItem {..}) = do
    H.toMarkup tiDescription
    H.br
    button H.! A.style "font-weight: lighter; display: block;"
   where
    button = H.span $ " Mark this item: " >> case tiState of
      Types.Todo       -> H.button "Todo -> Done"
      Types.InProgress -> H.button "InProgress -> Done"
      Types.Done       -> Shared.spaceElems
        [H.button "Done -> Todo", H.button "Done -> InProgress"]

newtype ROView resource = ROView resource


type TodoListRO = ROView Types.TodoList

instance H.ToMarkup TodoListRO where
  toMarkup (ROView tl) = todoListInvariantMarkup tl $ H.br >> Shared.titledList
    (H.h3 "Items")
    (ROView <$> Types.tlItems tl)

instance H.ToMarkup (ROView Types.TodoItem) where
  toMarkup (ROView Types.TodoItem {..}) = H.toMarkup tiDescription >> roMsg
   where
    roMsg = (H.span " (Item in Read-only mode)") H.! A.style
      "font-weight: lighter; display: block; background-color: bisque;"

-- | The part of the TodoList markup that doesn't change; along with some additional markup.
todoListInvariantMarkup :: Types.TodoList -> H.Markup -> H.Markup
todoListInvariantMarkup Types.TodoList {..} trailing = H.div $ do
  H.h2 $ H.toMarkup @Text tlName
  H.br
  H.toMarkup numItems
  trailing
  where numItems = T.unwords [show $ length tlItems, "items"]

