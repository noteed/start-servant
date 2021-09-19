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

type Todos = Get '[B.HTML] (Page 'Authd UP.UserTodos)

type TodosC m
  = (Applicative m, MonadError Rt.RuntimeErr m, S.DBStorage m TodoList)

todosT :: forall m . TodosC m => User -> ServerT Todos m
todosT authdUser = userTodos
 where
  userTodos = do
    lists <- S.dbSelect $ TodoListsByNamespace (authdUser ^. uUsername)
    let summaries = UP.TodoListSummary <$> lists
    pure . AuthdPage authdUser . UP.UserTodos $ summaries
