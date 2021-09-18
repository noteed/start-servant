{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
  , TodoListId(..)
  , TodoList(..)
  , TodoItem(..)
  , TodoState(..)
  , Operation(..)
  , Namespace(..)
  , Profile(..)
  , Credentials(..)
  , User(..)
  , uUsername
  , uEmail
  , uUserTagRels
  , uUserGroups
  , Session(..)

  -- * User operations
  , S.DBSelect(..)
  , S.DBUpdate(..)

  -- ** Errors
  , UserErr(..)
  ) where

import           Control.Lens
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Types
import           Prototype.ACL
import           Prototype.Runtime.Errors       ( IsRuntimeErr(..) )
import qualified Prototype.Runtime.Storage     as S
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

newtype TodoListId = TodoListId { _unTodoListId :: NonEmptyText }
                   deriving ( Eq
                            , Show
                            , IsString
                            , ToMarkup
                            , ToHttpApiData
                            , FromHttpApiData
                            , Hashable
                            ) via NonEmptyText

data TodoList = TodoList
  { tlName  :: Text
  , tlItems :: [TodoItem]
  , tlTags  :: Set Tag
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Resource TodoList where
  resourceTags = tlTags

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
  { namespace   :: Namespace
  , email       :: Text
  , name        :: Text
  , profGroups  :: Set GroupId
  , profTagRels :: TagRels
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

makeLensesFor [("username", "uUsername")
              , ("email", "uEmail")
              , ("userGroups", "uUserGroups")
              , ("userTagRels", "uUserTagRels")
              ] ''User

instance S.DBIdentity User where
  type DBId User = Namespace
  dbId = username

instance S.DBStorageOps User where
  -- | Kinds of manipulating operations that can be performed on users.
  data DBUpdate User = CreateNewUser User
                     | DeactivateUser Namespace
                     | AddToGroups Namespace (Set GroupId)
  
  -- | Ways to select user(s)
  data DBSelect User = AuthUser Credentials

-- TODO: A user is a grantee, and a user may belong to groups.
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

data UserErr = AuthFailed Text
             | PermissionDenied Text
             deriving Show

instance IsRuntimeErr UserErr where
  errCode = errCode' . \case
    AuthFailed{}       -> "AUTH_FAILED"
    PermissionDenied{} -> "PERMISSION_DENIED"
    where errCode' = mappend "ERR.USER"
  httpStatus = \case
    AuthFailed{}       -> unauthorized401
    PermissionDenied{} -> forbidden403

  userMessage = Just . addMsg . \case
    AuthFailed       msg -> msg
    PermissionDenied msg -> msg
    where addMsg = sentence . mappend "Unable to authenticate: "






