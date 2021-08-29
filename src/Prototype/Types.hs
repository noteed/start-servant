{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Prototype.Types where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Servant.Auth.Server            ( FromJWT
                                                , ToJWT
                                                )
import           Text.Blaze                     ( ToMarkup(toMarkup) )
import qualified Text.Blaze.Html5              as H
import           Web.FormUrlEncoded             ( FromForm )

--------------------------------------------------------------------------------
newtype Counter = Counter Int

type TodoListId = String

data TodoList = TodoList
  { tlName  :: String
  , tlItems :: [TodoItem]
  }
  deriving (Show, Read, Generic)

instance ToJSON TodoList
instance FromJSON TodoList

data TodoItem = TodoItem
  { tiDescription :: String
  , tiState       :: TodoState
  }
  deriving (Show, Read, Generic)

instance ToJSON TodoItem
instance FromJSON TodoItem

data TodoState = Todo | InProgress | Done
  deriving (Show, Read, Generic)

instance ToJSON TodoState
instance FromJSON TodoState


--------------------------------------------------------------------------------
data Operation = BumpCounter


--------------------------------------------------------------------------------
-- A profile is a complete account, not necessarily matched to a real person.
-- This could e.g. match an organization, a "system user", or a generic "ghost
-- user" where all deleted real users are sent.
data Profile = Profile
  { namespace :: String
  , email     :: String
  , name      :: String
  }
  deriving (Show, Read, Generic)

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
newtype Session = Session { username :: String }
  deriving (Eq, Generic, Ord)

instance ToJSON Session
instance FromJSON Session


--------------------------------------------------------------------------------
-- The signed data we write to/read from cookies. Should not be used outside
-- the authentication layer.
data User = User
  { username :: String
  , email    :: String
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance FromJSON User

instance ToJWT User
instance FromJWT User

-- The data we need to authenticate a user (then create a cookie with a User in
-- it).
data Credentials = Credentials
  { username :: String
  , password :: String
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Credentials
instance FromJSON Credentials

instance FromForm Credentials
