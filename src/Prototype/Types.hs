{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Prototype.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT, ToJWT)
import Text.Blaze (ToMarkup(toMarkup))
import qualified Text.Blaze.Html5 as H
import Web.FormUrlEncoded (FromForm)


--------------------------------------------------------------------------------
newtype Counter = Counter Int


--------------------------------------------------------------------------------
data Operation = BumpCounter


--------------------------------------------------------------------------------
-- A profile is a complete account, not necessarily matched to a real person.
-- This could e.g. match an organization, a "system user", or a generic "ghost
-- user" where all deleted real users are sent.
data Profile = Profile
  { namespace :: String
  , email :: String
  , name :: String
  }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Profile
instance FromJSON Profile


--------------------------------------------------------------------------------
-- Keep track of a logged in user. This must match a cookie with a User in it.
newtype Session = Session { username :: String }
  deriving (Eq, Generic, Ord)

instance ToJSON Session
instance FromJSON Session

-- In addition of `lookupProfile`, we can call this function to make sure a
-- session was created.
lookupSession :: User -> [Session] -> Maybe Session
lookupSession user sessions = case filter f sessions of
  [s] -> Just s
  _ -> Nothing
  where
  f session =
    username (session :: Session) == username (user :: User)


--------------------------------------------------------------------------------
-- The signed data we write to/read from cookies. Should not be used outside
-- the authentication layer.
data User = User { username :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance FromJSON User

instance ToJWT User
instance FromJWT User

-- The data we need to authenticate a user (then create a cookie with a User in
-- it).
data Credentials = Credentials { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Credentials
instance FromJSON Credentials

instance FromForm Credentials


--------------------------------------------------------------------------------
-- Convert the submitted login Credentials to a Profile.
authenticateProfile :: Credentials -> [(String, Profile)] -> Maybe Profile
authenticateProfile credentials profiles = case filter f profiles of
  [(_, p)] -> Just p
  _ -> Nothing
  where
  f (pw, profile) =
    namespace (profile :: Profile) == username (credentials :: Credentials) &&
    pw == password (credentials :: Credentials)

-- Convert a User (taken from a signed cookie) to a Profile.
lookupProfile :: User -> [(String, Profile)] -> Maybe Profile
lookupProfile user profiles = case filter f profiles of
  [(_, p)] -> Just p
  _ -> Nothing
  where
  f (_, profile) =
    namespace (profile :: Profile) == username (user :: User)
