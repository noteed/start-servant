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
  -- $commonPages
  , LoginPage(..)
  , SignupPage(..)
  ) where

import           Control.Lens
import           Prototype.Types
import qualified Text.Blaze                    as B

-- | Status of the authentication 
data AuthStat = Authd | Public

-- | The page: a page can be authenticated or not authenticated. We guarantee that with types. 
data Page (authStat :: AuthStat) page where
  -- | A page where a user information is available: the user is the authenticated user. 
  AuthdPage ::B.ToMarkup page => User -> page -> Page 'Authd page
  -- | A public page; no user information is necessary: eg. a login page.  
  PublicPage ::B.ToMarkup page => page -> Page 'Public page

instance B.ToMarkup (Page 'Authd page) where
  toMarkup (AuthdPage user page) = do
    -- TODO: Render the user's information as a navbar; in the future we'd like to add groups etc. the user belongs to here.
    -- render some sort of a divider between the navbar and the rest of the page contents. 
    navbar

    B.toMarkup page
    where navbar = B.toMarkup @Text $ "Hi! " <> (user ^. uEmail)

instance B.ToMarkup (Page 'Public page) where
  toMarkup (PublicPage page) = do
    -- TODO: proper navbar for unauthenticated pages.
    navbar
    B.toMarkup page
   where
    navbar = do
      "Login"
      "Signup"

-- $commonPages Commonly used pages.

data LoginPage = LoginPage

instance B.ToMarkup LoginPage where
  toMarkup _ = "Login page here"

data SignupPage = SignupPage

instance B.ToMarkup SignupPage where
  toMarkup _ = "TODO: Signup page here"
