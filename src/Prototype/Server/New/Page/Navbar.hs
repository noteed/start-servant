{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.Navbar
  ( Navbar(..)
  , IsNavbarContent(..)
  ) where

import           Control.Lens
import           Protolude
import           Prototype.Server.New.Page.Shared
import           Prototype.Types
import qualified Text.Blaze                    as B
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A


-- | A very basic navbar: contains the HTML contents that make up the navbar.  
newtype Navbar = Navbar { getNavbarHtml :: Html }
               deriving ToMarkup via Html

class IsNavbarContent a where

  -- | Indicate how we can render something as HTML.
  navbarMarkup :: a -> Html

instance IsNavbarContent User where
  navbarMarkup user =
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
