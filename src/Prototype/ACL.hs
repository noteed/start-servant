{-# LANGUAGE
    KindSignatures
  , DataKinds
#-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module: Prototype.ACL
Description: ACL permissions mock module.

The core ideas of this module have been inspired by: <https://tailscale.com/blog/rbac-like-it-was-meant-to-be/ RBAC as it was meant to be>

As the reader may guess, the right permission model is the one that suits a particular use case.
And we're probably not going to adhere to the recommendations in the article word by word, but develop a model
that suits our use case.

-}
module Prototype.ACL
  (
    -- * Typeclasses describing properties
    -- ** Resources
    Resource(..)
  , ResourceOps(..)

  -- ** Grantees
  , Grantee(..)
  , GroupedGrantee(..)
  , GroupedGranteeOps(..)

  -- * Re-exports
  -- We should avoid re-exports, but here it makes sense for convenience.
  , module ACLTypes

  -- * Grouping
  , groupResources

  -- * Authorization
  , authorize
  , authorizeV
  , authorizeEither
  ) where

import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Prototype.ACL.Types           as ACLTypes
import qualified Prototype.Runtime.Errors      as Errs

{- | A tagged resource.

Resources can have 0 or more tags, and they can be further tagged or untagged.
-}
class Resource r where

  -- | A resource must be tagged; it can have 0 or 1 tags.
  resourceTags :: r -> Set Tag

-- | Operations on some resource in some @m@
class Resource r => ResourceOps m r where

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

-- | Operations on grantees in some @m@
class GroupedGrantee g => GroupedGranteeOps m g where

  -- | Add grantee to some groups
  granteeAddToGroups :: g -> Set GroupId -> m g

  -- | Remove grantee from some groups
  granteeRemoveFromGroups :: g -> Set GroupId -> m g

-- | Group resources as per the grantee tag relationships. 
groupResources
  :: forall grantee res f
   . (Grantee grantee, Resource res, Ord res, Foldable f)
  => grantee
  -> f res
  -> GroupedResources res
groupResources grantee (toList -> resources) = GroupedResources
  $ Map.map collectByTagRels granteeTags'
 where
  collectByTagRels userTags = Set.fromList
    [ r
    | (r, resTags) <- resourcesAndTags
    , not $ userTags `Set.disjoint` resTags
    ]
  granteeTags'     = granteeTags grantee
  resourcesAndTags = [ (r, tags) | r <- resources, let tags = resourceTags r ]

{- | Authorize a grantee over a particular reseource.

Logically, this flow can be broken down in the following:

1. Get all TagGrants the grantee has over this resource. 

2. Check if the super tag grants of the desired grant match any of the found tag-grants. 
-}
authorize
  :: forall (tg :: TagGrant) grantee res m
   . ( Grantee grantee
     , Resource res
     , MonadError Errs.RuntimeErr m
     , TagGrantValue tg
     )
  => grantee
  -> res
  -> m (ResourceAuth tg res)
authorize grantee = authorizeV grantee (tagGrantValue @tg)

{- | Same as `authorize` but using a `TagGrant` at the value level.
-}
authorizeV
  :: forall grantee res tg m
   . (Grantee grantee, Resource res, MonadError Errs.RuntimeErr m)
  => grantee
  -> TagGrant
  -> res
  -> m (ResourceAuth tg res)
authorizeV grantee tagGrant resource =
  let
    -- first get all the TagRels that have tags that match with at least one tag of the resource.
      overlappingTagRels = Map.filter hasResourceTags granteeTagRels
      -- These are all the grants the user has over this resource.
      matchingTagGrants  = Map.keysSet overlappingTagRels
  in  if not $ superTagGrants' `Set.disjoint` matchingTagGrants
        then pure $ ResourceAuth resource
        else accessDenied matchingTagGrants
 where
  hasResourceTags userTags = not $ resourceTags' `Set.disjoint` userTags
  -- All grants superior to the grant requested, if the user has any of these grants
  -- on the resource, we're good to allow the authorization.
  superTagGrants' = superTagGrants tagGrant
  granteeTagRels  = granteeTags grantee
  resourceTags'   = resourceTags resource
  accessDenied    = Errs.throwError' . AccessDenied tagGrant

-- | Try multiple authorizations fallbacks.
authorizeEither
  :: forall m res tgPref tgFallback
   . (MonadError Errs.RuntimeErr m, MonadLog AppName m)
  => m (ResourceAuth tgPref res) -- ^ Preferred authorization
  -> m (ResourceAuth tgFallback res) -- ^ Fallback authorization
  -> m (Either (ResourceAuth tgPref res) (ResourceAuth tgFallback res))
authorizeEither preferred fallback =
  (Left <$> preferred) `catchError` logErrorFallback
 where
  logErrorFallback err = logErr >> (Right <$> fallback)
    where logErr = error (Errs.displayErr err)
