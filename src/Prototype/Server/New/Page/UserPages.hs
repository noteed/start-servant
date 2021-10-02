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
  toMarkup (RWView tl) = todoListInvariantMarkup tl $ Shared.titledList
    H.hr
    (RWView . (tl ^. Types.tlId, ) <$> Types._tlItems tl)

instance H.ToMarkup (RWView (Types.TodoListId, Types.TodoItem)) where
  toMarkup (RWView (tlId, Types.TodoItem {..})) = do
    H.toMarkup _tiDescription
    H.br
    button ! A.style "font-weight: lighter; display: block;"
   where
    button = H.span $ Shared.spaceElemsWith H.br $ case _tiState of
      Types.Todo       -> [mkButton Types.Todo Types.Done]
      Types.InProgress -> [mkButton Types.InProgress Types.Done]
      Types.Done ->
        [mkButton Types.Done Types.Todo, mkButton Types.Done Types.InProgress]
    mkButton from' to' =
      let btnText = show from' <> " -> " <> show to'
          link'   = H.textValue $ T.intercalate
            "/"
            [ "/private/user/todos"
            , tlId ^. coerced
            , "item"
            , _tiId ^. Lens.to Types._unTodoItemId . unNonEmptyText
            , "mark"
            ]
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

