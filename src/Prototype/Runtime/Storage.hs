{-# LANGUAGE TypeFamilies #-}
{- |
Module: Prototype.Runtime.Storage
Description: Abstract Storage

The goal of these typeclasses is to abstract /how/ data is stored; and
only rely on some environment/monad @m@; for which we can implement
these instances.

The core functions here are just
- dbUpdate
- dbSelect

And these functions can be implemented more concretely with the
definition of the @m@ in which they operate. 

-}
module Prototype.Runtime.Storage
  ( DBIdentity(..)
  , DBStorage(..)
  , DBStorageOps(..)
  ) where

-- | A class with the properties indicating that something has some notion of a unique ID in a storage layer.
-- Eg. for a user, this can be the username/namespace, etc. 
class DBIdentity a where
  -- | The ID of the stored value, this value should be unique.
  type DBId a :: Type
  -- | Get the ID from a larger product type, for example.
  dbId :: a -> DBId a

-- | Operations that can be performed on users. 
class DBIdentity a => DBStorageOps a where
  -- | A sum type, usually, indicating the kinds of queries that can be run to get values. 
  data DBSelect a :: Type
  -- | A sum type, usually, indicating the kinds of queries that can be run to update/insert/delete values. 
  data DBUpdate a :: Type

-- | Same as `DBStorageOps`; but here we define /how/ we run the operations defined. 
class (DBIdentity a, DBStorageOps a) => DBStorage m a where
  -- | Execute an update, reporting the list of IDs that were affected due to it.
  dbUpdate :: DBUpdate a -> m [DBId a]

  -- | Execute a select, returning the rows that were matched by the query.
  dbSelect :: DBSelect a -> m [a]
