{-# LANGUAGE TemplateHaskell #-}
module Prototype.Grantee.Group
  (
    -- * Groups 
    Group(..)
  , gId
  , gName
  , gTags
  ) where

import           Control.Lens
import           Prototype.ACL
import qualified Prototype.ACL.Types           as ACLT

-- | A group: a collection of users. 
data Group = Group
  { _gId   :: ACLT.GroupId -- ^ ID of the Group, IDs must be unique and preferably generated.  
  , _gName :: Maybe Text -- ^ Name of the Group. Names are only used for user friendliness.
  , _gTags :: ACLT.TagRels -- ^ Set of tags owned by the group.
  }
  deriving (Eq, Show, Ord)

makeLenses ''Group

{- | A group is a grantee:

1. A group can own tags.

2. A group, in essence, is in itself.
-}
instance Grantee Group where
  granteeTags = _gTags
