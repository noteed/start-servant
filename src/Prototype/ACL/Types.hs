{-# LANGUAGE
    DerivingVia
  , DeriveGeneric
  , DeriveAnyClass
#-}
{-# LANGUAGE TemplateHaskell #-}
module Prototype.ACL.Types
  ( GroupId(..)
  , unGroupId
  -- * Permissions over tags.
  , TagGrant(..)

  -- * Tags
  , Tag(..)
  , unTag
  , TagRels

  -- * Resources
  , GroupedResources(..)
  , unGroupedResources
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Prototype.Types.NonEmptyText  as NE
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )

-- | ID of a group.
newtype GroupId = GroupId { _unGroupId :: NE.NonEmptyText }
                deriving ( Eq
                         , Show
                         , Ord
                         , ToJSON
                         , FromJSON
                         , ToHttpApiData
                         , FromHttpApiData
                         ) via NE.NonEmptyText

makeLenses ''GroupId

-- | A grant is a specific permission given over a tag.
data TagGrant =
  TagRead -- ^ Read all objects tagged with a particular tag
  | TagWrite -- ^ Write all objects tagged with a particular tag
  | TagOwn  -- ^ Own a tag itself; owning a tag gives a grantee all permissions over the tagged objects
            -- with the particular tag.
  deriving (Eq, Show, Read, Generic, Ord, Hashable)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | A Tag, on the other hand, is an arbitrary identifier that can be
-- attached to resources. The combination of a tag and a grant define the ACL of the resource.
newtype Tag = Tag { _unTag :: NE.NonEmptyText }
            deriving (Eq, Show, ToJSON, FromJSON, Ord, IsString) via NE.NonEmptyText

makeLenses ''Tag

{- $tagRels A set of tags and the permissions the grantee has over them.

  For example, if a grantee owns the tags "database" & "engineering", but can read
  objects tagged "report"; we'd have:

  @
  TagOwn: [ "database", "engineering" ]
  TagRead: [ "report" ]
  @
-}
type TagRels = Map TagGrant (Set Tag)

{- | Given the tags of the resource and the user's relationships to the tags, we may want to group resources
based on the user's tag-specific permissions.

Eg. if the tag of a resource is X, and the user has TagRead permissions on tag X; the user also has
read permissions over this resource.
-}
newtype GroupedResources resource = GroupedResources { _unGroupedResources :: Map TagGrant (Set resource) }
                                  deriving (Eq, Show)

makeLenses ''GroupedResources
