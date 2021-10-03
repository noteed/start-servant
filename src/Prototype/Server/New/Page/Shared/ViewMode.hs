{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Prototype.Server.New.Page.Shared.ViewMode
  ( ViewMode(..)
  , ToMarkupInMode(..)
  , View(..)
  ) where

import qualified Text.Blaze.Html5              as H

-- | View modes
data ViewMode =
  RW -- ^ Read-write mode 
  | RO  -- ^ Read-only mode. 

-- | Some data can be outputted differently based on the modes. Eg. a TodoList allows the user to edit
-- the list in RW mode, but not in RO mode. Here we present a consistent interface for the same. 
class ToMarkupInMode (viewMode :: ViewMode) contents where
  toMarkupInMode :: contents -> H.Markup

-- | A view, typed on the mode in which the contents are viewed. 
data View (viewMode :: ViewMode) contents where
  -- | Markup contents in RW mode, as long as an instance exists.
  RWView ::ToMarkupInMode 'RW contents => contents -> View 'RW contents
  -- | Markup contents in RO mode, as long as an instance exists.
  ROView ::ToMarkupInMode 'RO contents => contents -> View 'RO contents

instance H.ToMarkup (View viewMode contents) where
  toMarkup = \case
    RWView contents -> toMarkupInMode @ 'RW contents
    ROView contents -> toMarkupInMode @ 'RO contents
