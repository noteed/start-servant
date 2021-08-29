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
  ) where

import           Control.Lens
import           Data.Aeson
import qualified Prototype.Types.NonEmptyText  as NE
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )

 -- | ID of a group.
newtype GroupId = GroupId { _unGroupId :: NE.NonEmptyText }
                deriving (Eq, Show, Ord, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData) via NE.NonEmptyText

makeLenses ''GroupId

-- | A grant is a specific permission given over a tag. 
data TagGrant =
  TagRead -- ^ Read all objects tagged with a particular tag
  | TagWrite -- ^ Write all objects tagged with a particular tag 
  | TagOwn  -- ^ Own a tag itself; owning a tag gives a grantee all permissions over the tagged objects
            -- with the particular tag. 
  deriving (Eq, Show, Generic, Ord, Hashable)
  deriving anyclass (ToJSON, FromJSON)

-- | A Tag, on the other hand, is an arbitrary identifier that can be
-- attached to resources. The combination of a tag and a grant define the ACL of the resource.
newtype Tag = Tag { _unTag :: Text }
            deriving (Eq, Show, ToJSON, FromJSON, Ord, IsString) via Text

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

