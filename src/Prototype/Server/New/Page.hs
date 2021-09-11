{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Lens
import           Prototype.Types
import qualified Text.Blaze                    as B

-- | Status of the authentication 
data AuthStat = Authd | NotAuthd

-- | The page: a page can be authenticated or not authenticated. We guarantee that with types. 
data Page (authStat :: AuthStat) page where
  AuthdPage ::B.ToMarkup page => User -> page -> Page 'Authd page

instance B.ToMarkup (Page 'Authd page) where
  toMarkup (AuthdPage user page) = do
    -- TODO: Render the user's information as a navbar; in the future we'd like to add groups etc. the user belongs to here.
    -- render some sort of a divider between the navbar and the rest of the page contents. 
    navbar

    B.toMarkup page
    where navbar = B.toMarkup @Text $ "Hi! " <> (user ^. uEmail)
