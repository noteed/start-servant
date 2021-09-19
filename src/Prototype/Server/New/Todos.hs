{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

type Todos =
       Get '[B.HTML] (Page 'Authd UP.UserTodos)
       -- Get a single todo-list 
  :<|> Capture "todoListId" TodoListId :> Get '[B.HTML] (Page 'Authd (ACL.ResourceAuth TodoList 'ACL.TagRead))

type TodosC m
  = (Applicative m, MonadError Rt.RuntimeErr m, S.DBStorage m TodoList)

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
      Just tl -> ACL.authorize @'ACL.TagRead authdUser tl <&> AuthdPage authdUser   
      Nothing -> noList id

  noList = Rt.throwError' . NoSuchTodoList
