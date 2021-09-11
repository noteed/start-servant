{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Prototype.Server.New
  ( New
  , newT
  , new
  ) where

import qualified Prototype.Server.New.StartPage
                                               as SP
import           Servant.API
import           Servant.Auth.Server
import           Servant.Server

-- | The new API: it contains all routes, protected as well as unprotected. 
type New = SP.Unprotected :<|> SP.Protected

-- | Our new server; comprised of protected and unprotected routes. 
newT :: SP.LoginC mode m => ServerT New m
newT = SP.unprotectedT :<|> SP.protectedT

new
  :: forall mode m
   . SP.LoginC mode m
  => (forall a . m a -> Handler a) -- ^ A natural transformation that maps the `m` into the handler. 
  -> Server New -- ^ The server; mapped to the plain servant `Handler`
new natTrans = hoistServerWithContext
  (Proxy @New)
  (Proxy @'[CookieSettings , JWTSettings])
  natTrans
  newT
