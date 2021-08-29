{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
#-}
{-# LANGUAGE
    DerivingStrategies
  , DeriveAnyClass
  , DerivingVia 
  , DeriveGeneric 
  , GeneralizedNewtypeDeriving
#-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module: Prototype.Types.NonEmptyText
Description: Safe texts guaranteed to be non-empty
-}
module Prototype.Types.NonEmptyText
  ( NonEmptyText'(..)
  , NonEmptyText
  , nonEmptyText
  , unNonEmptyText
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Text                     as T
import           Servant.API                    ( FromHttpApiData(..)
                                                , ToHttpApiData
                                                )

-- * NonEmptyText

-- ** Types and aliases

-- | `NonEmptyText` is `NonEmpty` specialised to `Text`.
newtype NonEmptyText' t = NonEmptyText { _unNonEmptyText :: t }
                        deriving stock (Eq, Generic, Ord)
                        deriving newtype (ToJSON, ToJSONKey, FromJSONKey, Hashable, ToHttpApiData)
                        deriving Show via t

type NonEmptyText = NonEmptyText' Text

makeLenses ''NonEmptyText'

instance FromHttpApiData NonEmptyText where
  parseUrlPiece = maybe (Left "Text cannot be empty") Right . nonEmptyText

-- | Safe constructor
-- Leading and trailing whitespace is stripped from a Just value.
nonEmptyText :: Text -> Maybe NonEmptyText
nonEmptyText t' | T.null t  = Nothing
                | otherwise = Just . NonEmptyText $ t' -- when the stripped text is non-empty; we'd like to keep the whitespaces; if any.
  where t = T.strip t'

-- | TODO: improve this with proper error messages. 
instance FromJSON NonEmptyText where
  parseJSON = \case
    String t -> maybe mempty pure . nonEmptyText $ t
    _        -> mempty
