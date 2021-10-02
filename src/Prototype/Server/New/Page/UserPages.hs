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

import           Control.Lens                  as Lens
import qualified Data.Text                     as T
import qualified Prototype.ACL                 as ACL
import qualified Prototype.Server.New.Page.Shared
                                               as Shared
import qualified Prototype.Types               as Types
import           Prototype.Types.NonEmptyText
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

newtype UserGroups = UserGroups (Set ACL.GroupId)

instance H.ToMarkup UserGroups where
  toMarkup (UserGroups ids) = Shared.titledList "Your groups" ids

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

newtype RWView resource = RWView resource

type TodoListRW = RWView Types.TodoList

instance H.ToMarkup TodoListRW where
  toMarkup (RWView tl) = todoListInvariantMarkup tl $ do
    H.hr
    H.h6 "Add an item to this list"
    addItemForm $ tl ^. Types.tlId
    Shared.titledList H.hr (RWView . (tl ^. Types.tlId, ) <$> Types._tlItems tl)

-- | Generate an add TodoItem form
addItemForm tlId = H.form (inputs >> submit) ! A.formaction link' ! A.method
  "POST"
 where
  inputs = do
    H.input ! A.name "_tiDescription" ! A.type_ "text"
    H.input
      ! A.name "_tiState"
      ! A.value (H.textValue $ show Types.Todo)
      ! A.type_ "text"
  submit =
    H.button "Add" ! A.type_ "submit" ! A.formaction link' ! A.formmethod "POST"
  link' = H.textValue $ T.intercalate "/" [userTodoPath tlId, "item", "create"]

instance H.ToMarkup (RWView (Types.TodoListId, Types.TodoItem)) where
  toMarkup (RWView (tlId, Types.TodoItem {..})) = do
    H.toMarkup _tiDescription
    H.br
    buttons ! A.style "font-weight: lighter; display: block;"
   where
    buttons = H.span $ Shared.spaceElemsWith H.br $ mkDelete : case _tiState of
      Types.Todo       -> [mkChangeState Types.Todo Types.Done]
      Types.InProgress -> [mkChangeState Types.InProgress Types.Done]
      Types.Done ->
        [ mkChangeState Types.Done Types.Todo
        , mkChangeState Types.Done Types.InProgress
        ]
    mkChangeState from' to' =
      let
        btnText = show from' <> " -> " <> show to'
        link' =
          H.textValue $ T.intercalate "/" [userTodoItemPath tlId _tiId, "mark"]
        button' =
          H.button (H.text btnText)
            ! A.type_ "submit"
            ! A.formaction link'
            ! A.method "PUT"
        input' =
          H.input
            ! A.name "newState"
            ! A.value (H.textValue $ show to')
            ! A.type_ "hidden"
      in
        -- FIXME: The form methods seem to get ignored, so while we're setting the method here, it has no affect.
        H.form (input' >> button') ! A.formaction link' ! A.method "PUT"

    mkDelete =
      let
        btnText = "Delete"
        link'   = H.textValue
          $ T.intercalate "/" [userTodoItemPath tlId _tiId, "delete"]
        button' =
          H.button (H.text btnText)
            ! A.type_ "submit"
            ! A.action link'
            ! A.method "DELETE"
      in
        -- FIXME: The form methods seem to get ignored, so while we're setting the method here, it has no affect.
        H.form button' ! A.action link' ! A.method "DELETE"

userTodoPath tlId = T.intercalate "/" ["/private/user/todos", tlId ^. coerced]
userTodoItemPath tlId tiId =
  T.intercalate "/" [userTodoPath tlId, "item", tiId ^. coerced]

newtype ROView resource = ROView resource
type TodoListRO = ROView Types.TodoList

instance H.ToMarkup TodoListRO where
  toMarkup (ROView tl) = todoListInvariantMarkup tl
    $ Shared.titledList H.hr (ROView <$> Types._tlItems tl)

instance H.ToMarkup (ROView Types.TodoItem) where
  toMarkup (ROView Types.TodoItem {..}) = H.toMarkup _tiDescription >> roMsg
   where
    roMsg = (H.span " (Item in Read-only mode)") ! A.style
      "font-weight: lighter; display: block; background-color: bisque;"

-- | The part of the TodoList markup that doesn't change; along with some additional markup.
todoListInvariantMarkup :: Types.TodoList -> H.Markup -> H.Markup
todoListInvariantMarkup Types.TodoList {..} trailing = H.div $ do
  H.h2 $ H.toMarkup @Text _tlName
  H.br
  H.h3 $ H.toMarkup numItems
  trailing
  where numItems = T.unwords [show $ length _tlItems, "items"]

