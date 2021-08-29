{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE
    DeriveGeneric
  , DerivingVia 
  , DeriveAnyClass
#-}
{-# LANGUAGE DuplicateRecordFields #-}

module Prototype.Types
  ( Counter(..)
  , TodoListId
  , TodoList(..)
  , TodoItem(..)
  , TodoState(..)
  , Operation(..)
  , Namespace(..)
  , Profile(..)
  , Credentials(..)
  , User(..)
  , Session(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Prototype.ACL
import           Prototype.Types.NonEmptyText
import           Prototype.Types.Secret
import           Servant.API                    ( FromHttpApiData
                                                , ToHttpApiData
                                                )
import           Servant.Auth.Server            ( FromJWT
                                                , ToJWT
                                                )
import           Text.Blaze                     ( ToMarkup(toMarkup) )
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm )

--------------------------------------------------------------------------------
newtype Counter = Counter Int

type TodoListId = Text

data TodoList = TodoList
  { tlName  :: Text
  , tlItems :: [TodoItem]
  }
  deriving (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TodoItem = TodoItem
  { tiDescription :: Text
  , tiState       :: TodoState
  }
  deriving (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TodoState = Todo | InProgress | Done
  deriving (Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
data Operation = BumpCounter

newtype Namespace = Namespace { _unNamespace :: NonEmptyText }
                  deriving ( Eq
                           , Show
                           , IsString
                           , ToJSON
                           , FromJSON
                           , ToMarkup
                           , H.ToValue
                           , Semigroup
                           , Hashable
                           , Ord
                           , FromHttpApiData
                           , ToHttpApiData
                           ) via NonEmptyText

--------------------------------------------------------------------------------
-- A profile is a complete account, not necessarily matched to a real person.
-- This could e.g. match an organization, a "system user", or a generic "ghost
-- user" where all deleted real users are sent.
data Profile = Profile
  { namespace :: Namespace
  , email     :: Text
  , name      :: Text
  }
  deriving (Show, Generic)

instance ToJSON Profile
instance FromJSON Profile

htmlProfile Profile {..} = H.div $ do
  H.div $ do
    "Display name: "
    H.toHtml name
  H.div $ do
    "Username: "
    H.toHtml namespace
  H.div $ do
    "Email: "
    H.toHtml email

instance ToMarkup Profile where
  toMarkup = htmlProfile



--------------------------------------------------------------------------------
-- Keep track of a logged in user. This must match a cookie with a User in it.
newtype Session = Session { username :: Namespace }
  deriving (Eq, Generic, Ord)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- The signed data we write to/read from cookies. Should not be used outside
-- the authentication layer.
data User = User
  { username    :: Namespace -- ^ User's unique namespace/id
  , email       :: Text -- ^ TODO: newtype 
  , userGroups  :: Set GroupId -- ^ Set of groups the user belongs to 
  , userTagRels :: TagRels -- ^ Users direct tag relationships (eg. tags owned, read, written)
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJWT, FromJWT)

-- TODO: A user is a grantee, and an user may belong to groups. 
instance Grantee User where
  granteeTags = userTagRels

instance GroupedGrantee User where
  granteeGroups = userGroups

-- The data we need to authenticate a user (then create a cookie with a User in
-- it).
data Credentials = Credentials
  { username :: Namespace
  , password :: Secret '[ 'ToJSONExp] Text -- ^ Password, as secret.
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, FromForm)
