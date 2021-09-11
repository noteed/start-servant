{-# LANGUAGE
    KindSignatures
  , DataKinds 
#-}
{- |
Module: Prototype.Server.New.Page
Description: Pages with authentication information at the type-level
-}
module Prototype.Server.New.Page
  ( Page(..)
  , AuthStat(..)
  ) where

import           Prototype.Types
import qualified Text.Blaze                    as B

-- | Status of the authentication 
data AuthStat = Authd | NotAuthd

-- | The page: a page can be authenticated or not authenticated. We guarantee that with types. 
data Page (authStat :: AuthStat) page where
  AuthdPage ::B.ToMarkup page => User -> page -> Page 'Authd page

