{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE
    KindSignatures
  , DataKinds
#-}
{-# LANGUAGE
    DerivingVia
  , DeriveGeneric
  , DeriveAnyClass
#-}
module Prototype.ACL.Types
  ( GroupId(..)
  , unGroupId

  -- * Permissions over tags.
  , TagGrant(..)
  , TagGrantValue(..)
  , subTagGrants
  , superTagGrants

  -- * Tags
  , Tag(..)
  , unTag
  , TagRels

  -- * Resources
  , GroupedResources(..)
  , unGroupedResources

  -- * Representations of successful authorization
  , ResourceAuth(..)
  , raResource

  -- * Errors
  , ACLErr(..)
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Network.HTTP.Types            as HTTP
import qualified Prototype.Runtime.Errors      as Errs
import qualified Prototype.Types.NonEmptyText  as NE
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )
import qualified Text.Blaze.Html5              as H

-- | ID of a group.
newtype GroupId = GroupId { _unGroupId :: NE.NonEmptyText }
                deriving ( Eq
                         , Show
                         , Ord
                         , ToJSON
                         , FromJSON
                         , ToHttpApiData
                         , FromHttpApiData
                         , H.ToMarkup
                         ) via NE.NonEmptyText

makeLenses ''GroupId

{- | A grant is a specific permission given over a tag.

TagGrants are ordered: eg. @TagOwn > TagWrite@ holds, which also means if one has `TagOwn` over a particular tag, they also have `TagWrite`. 
-}
data TagGrant =
  TagRead -- ^ Read all objects tagged with a particular tag
  | TagWrite -- ^ Write all objects tagged with a particular tag
  | TagOwn  -- ^ Own a tag itself; owning a tag gives a grantee all permissions over the tagged objects
            -- with the particular tag.
  deriving (Eq, Show, Read, Generic, Ord, Enum, Bounded, Hashable)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- | Tags, using their Ord instance, can /bundle/ other tags that are logically "less-than" the given tag.
-- For example; `TagOwn` bundles `TagWrite` and `TagRead`; and `TagWrite` bundles `TagRead`. 
-- 
-- (Includes the given tag)
subTagGrants :: TagGrant -> Set TagGrant
subTagGrants = Set.fromList . enumFromTo minBound

-- | Super `TagGrant`s of a given tag (including the given tag)
superTagGrants :: TagGrant -> Set TagGrant
superTagGrants = Set.fromList . (`enumFromTo` maxBound)

{- | A typeclass to map a TagGrant at the type level (a kind) to the value level.

This is unfortunately required if we want to map from the type-level to the value level.

However, the use of the typeclass is closed: one can include this as a constraint with full guarantees that this constraint will not
lead to requirements of implementing any more typeclass instances than the ones in this module.
-}
class TagGrantValue (tg :: TagGrant) where
  tagGrantValue :: TagGrant

instance TagGrantValue 'TagRead where
  tagGrantValue = TagRead

instance TagGrantValue 'TagWrite where
  tagGrantValue = TagWrite

instance TagGrantValue 'TagOwn where
  tagGrantValue = TagOwn

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

-- | A datatype representing successful authentication.
newtype ResourceAuth res (tg :: TagGrant) = ResourceAuth { _raResource :: res }
                                              deriving (Eq, Show)

makeLenses ''ResourceAuth

data ACLErr = AccessDenied TagGrant (Set TagGrant)
  deriving (Eq, Show)

instance Errs.IsRuntimeErr ACLErr where
  errCode = errCode' . \case
    AccessDenied{} -> "ACCESS_DENIED"
    where errCode' = mappend "ERR.ACL."

  httpStatus = \case
    AccessDenied{} -> HTTP.forbidden403

  userMessage = Just . \case
    AccessDenied wanted found -> T.unwords
      [ "No access to resource. Desired grant:"
      , show wanted
      , "available grants:"
      , show found
      ]
