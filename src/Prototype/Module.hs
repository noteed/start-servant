{-# LANGUAGE OverloadedStrings #-}
{- |

Module: Prototype.Module
Description: Get information about current module, etc.

This is a slightly improved version of
<https://stackoverflow.com/a/5679470/1360368 this> SO answer.

-}
module Prototype.Module (moduleOf) where

import qualified Data.Text as T
import Language.Haskell.TH

-- | Get the name of the module.

moduleOf :: Name -> Maybe Text
moduleOf = dropLastToken . show

dropLastToken :: Text -> Maybe Text
dropLastToken =
   fmap (T.intercalate "." ) . initMay . T.splitOn "."
