{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
module Prototype.Lib.Wrapped
  ( Wrapped(..)
  , fieldNameT
  ) where

import           Data.Aeson
import qualified Data.Text                     as T
import           GHC.TypeLits
import           Protolude
import qualified Web.FormUrlEncoded            as Form
import           Web.HttpApiData

{- | A wrapped newtype is just a representation of another type, wrapped in a field at the type level. 

Useful for deriving instances using instances of the underlying @t@ that can be wrapped in a particular "field". 

Consider that we have @FromJSON Text@; we can also have @FromJSON (Wrapped "field" Text)@ where the value of type Text will be read from a
JSON object at field @"field"@

Note that the FromHttpApiData & ToHttpApiData instances are derived via @t@ (they are the same instances) since
there is no notion of fields in these instances. 
-}
newtype Wrapped (fieldName :: Symbol) t
  = Wrapped { unWrap :: t }
  deriving (Eq, Show)
  deriving (FromHttpApiData, ToHttpApiData) via t

-- | Given we know how to parse out a @t@ from Http data, we should also be able to read it at a given
-- @fieldName@ from HTTP form-data.
instance (KnownSymbol fieldName, FromHttpApiData t) => Form.FromForm (Wrapped fieldName t) where
  fromForm = fmap Wrapped . Form.parseUnique @t (fieldNameT @fieldName)

{- | Output a wrapped value as a JSON object with the supplied Field name. 

Example:

>>> encode (Wrapped "Bar" :: Wrapped "foo" Text)
{ "foo" : "Bar" }  

-}
instance (KnownSymbol fieldName, ToJSON t) => ToJSON (Wrapped fieldName t) where
  toJSON (Wrapped t) = object [fieldNameT @fieldName .= t]

-- | Inverse of the @FromJSON@ instance above.  
instance (KnownSymbol fieldName, FromJSON t) => FromJSON (Wrapped fieldName t) where
  parseJSON = withObject ("JSON object with field: " <> T.unpack fieldName)
    $ \obj -> obj .: fieldName <&> Wrapped
    where fieldName = fieldNameT @fieldName

fieldNameT :: forall fieldName . KnownSymbol fieldName => Text
fieldNameT = T.pack . symbolVal $ Proxy @fieldName
