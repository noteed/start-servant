{- |
Module: Prelude
Description: Custom prelude for start-servant

We import Protolude and re-export it, for the usual protolude niceties;
and this module is meant to add more such re-exports as necessary as the project progresses.

-}
module Prelude
  ( module Proto
  , module Def
  ) where

import           Data.Default.Class            as Def
import           Protolude                     as Proto
