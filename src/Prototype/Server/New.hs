{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Prototype.Server.New
  ( New
  ) where

import qualified Prototype.Server.New.StartPage
                                               as SP
import           Prototype.Types                ( User(..) )
import           Servant.API
import           Servant.Server

type New = SP.Unprotected :<|> SP.Protected
