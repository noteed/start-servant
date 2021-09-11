{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Prototype.Server.New
  () where

import           Prototype.Server.New.StartPage ( Protected )
import           Prototype.Types                ( User(..) )
import           Servant.API
import           Servant.Server

-- type New' = SP.Unprotected :<|> SP.Protected
