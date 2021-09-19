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

import qualified Prototype.ACL as ACL 
import           Control.Lens
import qualified Prototype.Runtime.Errors      as Rt
import qualified Prototype.Runtime.Storage     as S
import           Prototype.Server.New.Page
import qualified Prototype.Server.New.Page.UserPages
                                               as UP
import           Prototype.Types
import           Servant.API
import qualified Servant.HTML.Blaze            as B
import           Servant.Server

type TodoListModes = PageEither (ACL.ResourceAuth 'ACL.TagWrite UP.TodoListRW) (ACL.ResourceAuth 'ACL.TagRead UP.TodoListRO) 

type Todos =
       Get '[B.HTML] (Page 'Authd UP.UserTodos)
       -- Get a single todo-list 
  :<|> Capture "todoListId" TodoListId :> Get '[B.HTML] (Page 'Authd TodoListModes)

type TodosC m
  = (Applicative m, MonadError Rt.RuntimeErr m, S.DBStorage m TodoList, MonadLog AppName m)

todosT :: forall m . TodosC m => User -> ServerT Todos m
todosT authdUser =
  userTodos
  :<|> viewTodo
 where
  userTodos = do
    lists <- S.dbSelect $ TodoListsByNamespace (authdUser ^. uUsername)
    let summaries = UP.TodoListSummary <$> lists
    pure . AuthdPage authdUser . UP.UserTodos $ summaries

  viewTodo id = do
    lists <- S.dbSelect $ TodoListsByNamespace (authdUser ^. uUsername)
    case find ((== id) . tlId) lists of 
      Just tl ->
        let asRO = ACL.authorize @'ACL.TagRead authdUser tl 
            asRW = ACL.authorize @'ACL.TagWrite authdUser tl 
        in AuthdPage authdUser . pageEither . bimap (fmap UP.RWView) (fmap UP.ROView) <$> ( asRW `ACL.authorizeEither` asRO  )
      Nothing -> noList id

  noList = Rt.throwError' . NoSuchTodoList
