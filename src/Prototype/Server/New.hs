{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Prototype.Server.New
  ( New
  , newT
  , new
  -- * Running the server. 
  , mkApplication
  ) where

import           Control.Lens            hiding ( Context )
import qualified Prototype.Runtime             as Rt
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

-- | Reduce a @ServerT New m@ to a @Server New Handler@ (via a natural transformation)
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

-- | Wire everything up and make a Wai `Application` out of it.
mkApplication
  :: forall mode m
   . SP.LoginC mode m
  => Rt.Runtime mode
  -> (forall a . m a -> Handler a) -- ^ A natural transformation that maps the `m` into the handler. 
  -> Application
mkApplication Rt.Runtime {..} natTrans = serveWithContext (Proxy @New)
                                                          ctx
                                                          (new natTrans)
  where ctx = (_rConf ^. Rt.cCookieSettings) :. _rJwtSettings :. EmptyContext
