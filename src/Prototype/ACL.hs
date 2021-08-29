{-# LANGUAGE StrictData #-}
{-# LANGUAGE
    DerivingStrategies
  , DeriveAnyClass
  , DerivingVia
  , DeriveGeneric
#-}
{- |
Module: Prototype.ACL
Description: ACL permissions mock module.

The core ideas of this module have been inspired by: <https://tailscale.com/blog/rbac-like-it-was-meant-to-be/ RBAC as it was meant to be>

As the reader may guess, the right permission model is the one that suits a particular use case.
And we're probably not going to adhere to the recommendations in the article word by word, but develop a model
that suits our use case.

-}
module Prototype.ACL
  (  -- * Typeclasses describing properties
    Resource(..)
  , Grantee(..)
  , GroupedGrantee(..)
  ) where

import           Prototype.ACL.Types

{- | A tagged resource.

Resources can have 0 or more tags, and they can be further tagged or untagged.
-}
class Resource r where

  -- | A resource must be tagged; it can have 0 or 1 tags.
  resourceTags :: r -> Set Tag

  -- | Tag a resource with a set of additional tags/add tags to resource
  tagResource :: r -> Set Tag -> m r

  -- | Remove a set of tags from a resource.
  untagResource :: r -> Set Tag -> m r

-- | A grantee: a grantee can have permissions over resources.
-- A grantee can be a user, or a group.
class Grantee g where

  -- | Tags held by the grantee
  granteeTags :: g -> TagRels

-- | A grantee that is a member of some groups.
class Grantee g => GroupedGrantee g where

  -- | The groups the grantee belongs to.
  granteeGroups :: g -> Set GroupId

  -- | Add grantee to some groups
  granteeAddToGroups :: g -> Set GroupId -> g

  -- | Remove grantee from some groups
  granteeRemoveFromGroups :: g -> Set GroupId -> g
