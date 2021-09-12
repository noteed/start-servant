{-# LANGUAGE OverloadedStrings #-}
module Prototype.Server.New.Page.Shared
  ( inputField
  , pageHeading
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

