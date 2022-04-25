{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Prototype.Server.New.Todos
  ( Todos
  , TodosC
  , todosT
  ) where

import           Control.Lens
import qualified Logging
import           Protolude
import qualified Prototype.ACL                 as ACL
import qualified Prototype.Runtime.Errors      as Rt
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Server.New.Page
import qualified Prototype.Server.New.Page.Shared.ViewMode
                                               as VM
import qualified Prototype.Server.New.Page.UserPages
                                               as UP
import           Prototype.Types
import           Servant.API
import qualified Servant.HTML.Blaze            as B
import           Servant.Server

type TodoListModes
  = PageEither
      (ACL.ResourceAuth 'ACL.TagWrite UP.TodoListRW)
      (ACL.ResourceAuth 'ACL.TagRead UP.TodoListRO)

-- brittany-disable-next-binding 
type Todos =
       Get '[B.HTML] (Page 'Authd User UP.UserTodos)
  :<|> "new-list" :> ( "form" :> Get '[B.HTML] (Page 'Authd User UP.TodoListCreatePage)
                     :<|> "create" :> ReqBody '[FormUrlEncoded] TodoListCreate :> Post '[B.HTML] RWListPage
                     )
       -- Get a single todo-list 
  :<|> "existing-list" :>  Capture "todoListId" TodoListId :> ( Get '[B.HTML] (Page 'Authd User TodoListModes)
                                                                :<|> "item" :> Capture "todoItemId" TodoItemId :> (MarkItem :<|> DelItem)
                                                                :<|> "item" :> (CreateItem :<|> EditItem)
                                                              )

type RWListPage
  = Page 'Authd User (ACL.ResourceAuth 'ACL.TagWrite UP.TodoListRW)

-- brittany-disable-next-binding 
-- | Item mark EP (FIXME: method should be changed to PUT) 
type MarkItem =  "mark" :> QueryParam' '[Required] "newState" TodoState
                        :> Get '[B.HTML] RWListPage

-- brittany-disable-next-binding 
-- | Item delete EP (FIXME: method should be changed to DELETE) 
type DelItem
  = "delete" :> Get '[B.HTML] RWListPage

-- | Create item EP 
type CreateItem
  = "create" :> ReqBody '[FormUrlEncoded] TodoItemCreate :> Post '[B.HTML] RWListPage

-- | Create item EP 
type EditItem
  = "edit" :> ReqBody '[FormUrlEncoded] TodoItem :> Post '[B.HTML] RWListPage

type TodosC m
  = ( Applicative m
    , MonadError Rt.RuntimeErr m
    , S.DBStorage m TodoList
    , Logging.MonadLog Logging.AppName m
    )

todosT :: forall m . TodosC m => User -> ServerT Todos m
todosT authdUser = userTodos :<|> todoListCreation :<|> specificTodo
 where
  userTodos = do
    lists <- S.dbSelect $ TodoListsByNamespace (authdUser ^. uUsername)
    let summaries = UP.TodoListSummary <$> lists
    pure . AuthdPage authdUser . UP.UserTodos $ summaries

  specificTodo id = viewTodo :<|> specificItem :<|> (createItem :<|> editItem)
   where
    viewTodo = getTargetTodo >>= authorizeGetTodo
    specificItem itemId = markItem :<|> delItem
     where
      markItem itemState = withRW $ S.gettingAffectedFirstErr
        TodoListById
        (MarkItem id itemId itemState)

      delItem =
        withRW $ S.gettingAffectedFirstErr TodoListById (DeleteItem id itemId)

    createItem = withRW . S.gettingAffectedFirstErr TodoListById . AddItem id
    editItem   = withRW . S.gettingAffectedFirstErr TodoListById . EditItem id

    withRW update = do
        -- Authorize the user to be able to actually RW on this Todo list.
      authRW =<< getTargetTodo
      affList <- update
      AuthdPage authdUser . fmap VM.RWView <$> authRW affList


    -- Helper function: authorizes a user's access level for a TODO and gets it in the appropriate mode (RW or RO)
    authorizeGetTodo tl =
      let asRO = ACL.authorize @ 'ACL.TagRead authdUser tl
          asRW = ACL.authorize @ 'ACL.TagWrite authdUser tl
      in  AuthdPage authdUser
            .   pageEither
            .   bimap (fmap VM.RWView) (fmap VM.ROView)
            <$> (asRW `ACL.authorizeEither` asRO)

    getTargetTodo = do
      lists <- S.dbSelect $ TodoListsByNamespace (authdUser ^. uUsername)
      case find ((== id) . _tlId) lists of
        Just tl -> pure tl
        Nothing -> noList id

  todoListCreation = showForm :<|> createList
   where
    showForm   = pure $ AuthdPage authdUser UP.TodoListCreatePage
    createList = undefined

  authRW = ACL.authorize @ 'ACL.TagWrite authdUser

  noList = Rt.throwError' . NoSuchTodoList
