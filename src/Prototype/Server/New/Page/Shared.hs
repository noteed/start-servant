{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.Shared
  ( inputField
  , pageHeading
  , spaceElem
  , spaceElemWith
  , spaceElems
  , spaceElemsWith
  -- * Lists 
  , titledList
  ) where

import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5.Attributes   as A

-- | Generate an input field with name and type.
inputField name type' req =
  let i =
        H.input
          ! A.type_ type'
          ! A.name name
          ! A.placeholder name
          ! A.autocomplete "off"
  in  if req then i ! A.required "true" else i

-- TODO: Add some CSS stylesheets etc. here in the future. 
-- | Add a common page heading: sets up the CSS imports, necessary encoding values etc. 
pageHeading = H.docTypeHtml

-- | Space out an elem with a trailing pipe. 
spaceElem = (`spaceElemWith` (H.text " | "))

-- | Space out an elem with a trailing pipe. 
spaceElemWith separator l = l >> separator

-- | Space out a list of elems with trailing pipes interspersed. 
spaceElems :: Foldable f => f H.Html -> H.Html
spaceElems = spaceElemsWith (H.text " | ")

-- | Space out a list of elems with trailing pipes interspersed. 
spaceElemsWith :: Foldable f => H.Html -> f H.Html -> H.Html
spaceElemsWith separator elems = case toList elems of
  []         -> mempty
  [h       ] -> h
  (h : rest) -> spaceElemWith separator h >> spaceElems rest


titledList
  :: forall item f
   . (H.ToMarkup item, Foldable f)
  => H.Html
  -> f item
  -> H.Html
titledList title (toList -> items) = title >> H.br >> H.ul
  (sequence_ (dispItem <$> items))
  where dispItem = H.li . H.toMarkup

