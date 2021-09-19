{-# LANGUAGE
    TypeOperators
  , DataKinds
  , TypeFamilies
#-}
{- |
Module: Prototype.Server.New.Auth
Description: Authentication and authorization module.

-}
module Prototype.Server.New.Auth
  ( UserAuthentication
  , PostAuthHeaders
  ) where

import qualified Prototype.ACL                 as ACL
import           Prototype.Types
import           Servant.API
import qualified Servant.Auth.Server           as SAuth
import           Servant.Server.Experimental.Auth

-- brittany-disable-next-binding
-- | Simple user authentication.
type UserAuthentication = SAuth.Auth '[SAuth.Cookie] User

-- | Headers that will be returned post a successful authentication.
type PostAuthHeaders
  = '[ Header "Location" Text
     , Header "Set-Cookie" SAuth.SetCookie
     , Header "Set-Cookie" SAuth.SetCookie
     ]
