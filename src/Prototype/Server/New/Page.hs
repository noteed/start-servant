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
  ( PageEither(..)
  , pageEither
  , Page(..)
  , AuthStat(..)
  -- $commonPages
  , LoginPage(..)
  , SignupPage(..)
  ) where

import           Control.Lens
import           Protolude
import           Prototype.Server.New.Page.Shared
import           Prototype.Types
import qualified Text.Blaze                    as B
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | A page, with alternatives.
data PageEither pageL pageR where
  PageL ::H.ToMarkup pageL => pageL -> PageEither pageL pageR
  PageR ::H.ToMarkup pageR => pageR -> PageEither pageL pageR

pageEither
  :: forall pageL pageR
   . (H.ToMarkup pageL, H.ToMarkup pageR)
  => Either pageL pageR
  -> PageEither pageL pageR
pageEither = either PageL PageR

instance H.ToMarkup (PageEither pageL pageR) where
  toMarkup = \case
    PageL l -> H.toMarkup l
    PageR r -> H.toMarkup r

-- | Status of the authentication 
data AuthStat = Authd | Public

-- | The page: a page can be authenticated or not authenticated. We guarantee that with types. 
data Page (authStat :: AuthStat) page where
  -- | A page where a user information is available: the user is the authenticated user. 
  AuthdPage ::B.ToMarkup page => User -> page -> Page 'Authd page
  -- | A public page; no user information is necessary: eg. a login page.  
  PublicPage ::B.ToMarkup page => page -> Page 'Public page

instance B.ToMarkup (Page 'Authd page) where
  toMarkup (AuthdPage user page) = pageHeading . H.body $ do
    -- TODO: Render the user's information as a navbar; in the future we'd like to add groups etc. the user belongs to here.
    -- render some sort of a divider between the navbar and the rest of the page contents. 
    navbar

    B.toMarkup page
   where
    navbar =
      let
        greeting     = B.toMarkup @Text $ "Hi! " <> (user ^. uEmail)
        groupsLink   = H.a "Your groups" ! A.href "/private/user/groups"
        todosLink    = H.a "Your todos" ! A.href "/private/user/todos"
        settingsLink = H.a "Your settings" ! A.href "/private/user/settings"
        logoutLink   = H.a "Logout" ! A.href "/private/user/logout"
        allLinks =
          spaceElems [greeting, groupsLink, todosLink, settingsLink, logoutLink]
      in
        allLinks >> H.hr >> H.br

instance B.ToMarkup (Page 'Public page) where
  toMarkup (PublicPage page) = pageHeading . H.body $ do
    -- TODO: proper navbar for unauthenticated pages.
    navbar
    B.toMarkup page
    where navbar = spaceElems [signupLink] >> H.hr >> H.br

-- $commonPages Commonly used pages.

data LoginPage = LoginPage H.AttributeValue

instance B.ToMarkup LoginPage where
  toMarkup (LoginPage authPath) =
    let
      form = H.form $ do
        H.h1 "Please login"
        H.div (inputField "username" "text" True ! A.autofocus "on")
          ! A.class_ "form-group"
        H.br
        H.div (inputField "password" "password" True) ! A.class_ "form-group"
        H.br
        H.button "Submit"
          ! A.formaction authPath
          ! A.formmethod "POST"
          ! A.class_ "btn btn-primary"
    in  H.div form ! A.class_ "col-md-6"

data SignupPage = SignupPage

instance B.ToMarkup SignupPage where
  toMarkup _ = do
    "TODO"
    loginLink

loginLink = H.a (H.span "Login") ! A.href "/public/login"
signupLink = H.a (H.span "Signup") ! A.href "/public/signup"
