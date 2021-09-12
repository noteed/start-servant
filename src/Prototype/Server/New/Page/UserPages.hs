{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.UserPages
  ( UserGroups(..)
  ) where

import           Prototype.ACL
import qualified Text.Blaze.Html5              as H

newtype UserGroups = UserGroups (Set GroupId)

-- | TODO: properly format this
instance H.ToMarkup UserGroups where
  toMarkup (UserGroups ids) = do
    "Your groups"
    H.br
    H.ul allIds
   where
    allIds = sequence_ $ dispId <$> toList ids
    dispId gid = H.li . H.toMarkup @Text $ show gid
