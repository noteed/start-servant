{-# OPTIONS_GHC -fno-warn-orphans #-} -- done as a temporary measure, should be removed in the future.
                                      -- Orphan instances are a nuisance, and a giant flaw/drawback in the typeclass design. 
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages.Todos.Item
  (
  -- ** Specific to Todos
    TodoListRW
  , TodoListRO
  -- * Generating paths
  , userTodoPath
  , userTodoItemPath
  ) where

import           Control.Lens                  as Lens
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import           Protolude
import qualified Prototype.Server.New.Page.Shared
                                               as Shared
import qualified Prototype.Server.New.Page.Shared.ViewMode
                                               as VM
import qualified Prototype.Types               as Types
import           Prototype.Types.NonEmptyText
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

type TodoListRW = VM.View 'VM.RW Types.TodoList

-- Orphan, but we need to live with this until we have a properly divided Types module.
instance VM.ToMarkupInMode 'VM.RW Types.TodoList where
  toMarkupInMode tl = todoListInvariantMarkup tl $ do
    H.hr
    H.h6 "Add an item to this list"
    itemForm (tl ^. Types.tlId) Nothing "create" HTTP.POST
    Shared.titledList
      H.hr
      (VM.toMarkupInMode @ 'VM.RW . (tl ^. Types.tlId, ) <$> Types._tlItems tl)

-- | Generate an add TodoItem form
itemForm
  :: Types.TodoListId -> Maybe Text -> Text -> HTTP.StdMethod -> H.Markup
itemForm tlId itemId pathEnd formMethodStd =
  H.form (inputs >> submit) ! A.action link' ! A.method formMethod
 where
  inputs = do
    H.input ! A.name "_tiDescription" ! A.type_ "text"
    H.input
      ! A.name "_tiState"
      ! A.value (H.textValue $ show Types.Todo)
      ! A.type_ "text"
    when (isJust itemId) H.input
      ! A.name "_tiId"
      ! A.value (H.textValue $ fromMaybe "" itemId)
      ! A.type_ "hidden"
  submit =
    H.button (H.toMarkup $ T.toTitle pathEnd)
      ! A.type_ "submit"
      ! A.formaction link'
  link' = H.textValue $ T.intercalate "/" [userTodoPath tlId, "item", pathEnd]
  formMethod = H.textValue . decodeUtf8 . HTTP.renderStdMethod $ formMethodStd

-- Orphan, but we need to live with this until we have a properly divided Types module.
instance VM.ToMarkupInMode 'VM.RW (Types.TodoListId, Types.TodoItem) where
  toMarkupInMode (tlId, Types.TodoItem {..}) = do
    H.toMarkup _tiDescription
    H.br
    buttons ! A.style "font-weight: lighter; display: block;"
   where
    buttons =
      H.span
        $ Shared.spaceElemsWith H.br
        $ editItem
        : mkDelete
        : case _tiState of
            Types.Todo       -> [mkChangeState Types.Todo Types.Done]
            Types.InProgress -> [mkChangeState Types.InProgress Types.Done]
            Types.Done ->
              [ mkChangeState Types.Done Types.Todo
              , mkChangeState Types.Done Types.InProgress
              ]
    editItem = itemForm tlId (Just $ _tiId ^. coerced) "edit" HTTP.POST
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

userTodoPath :: Types.TodoListId -> Text
userTodoPath tlId =
  T.intercalate "/" ["/private/user/todos/existing-list", tlId ^. coerced]

userTodoItemPath tlId tiId =
  T.intercalate "/" [userTodoPath tlId, "item", tiId ^. coerced]

type TodoListRO = VM.View 'VM.RO Types.TodoList

instance VM.ToMarkupInMode 'VM.RO Types.TodoList where
  toMarkupInMode tl = todoListInvariantMarkup tl $ Shared.titledList
    H.hr
    (VM.toMarkupInMode @ 'VM.RO <$> Types._tlItems tl)

instance VM.ToMarkupInMode 'VM.RO Types.TodoItem where
  toMarkupInMode Types.TodoItem {..} = H.toMarkup _tiDescription >> roMsg
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

