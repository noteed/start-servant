{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages
  ( -- * Groups
    UserGroups(..)
  -- * Todos 
  , module TodoPages
  ) where

import qualified Data.Set                      as Set
import qualified Prototype.ACL                 as ACL
import qualified Prototype.Server.New.Page.Shared
                                               as Shared
import           Prototype.Server.New.Page.UserPages.Todos
                                               as TodoPages
import qualified Text.Blaze.Html5              as H

newtype UserGroups = UserGroups (Set.Set ACL.GroupId)

instance H.ToMarkup UserGroups where
  toMarkup (UserGroups ids) = Shared.titledList "Your groups" ids

