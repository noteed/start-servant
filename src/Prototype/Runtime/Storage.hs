{-# LANGUAGE OverloadedStrings #-}
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
  -- * Performing updates and getting values. 
  , gettingAffected
  , gettingAffectedFirstMaybe
  , gettingAffectedFirstErr
  ) where

import qualified Data.Text                     as T
import qualified Data.Typeable                 as Typeable
import qualified Network.HTTP.Types.Status     as Status
import           Prototype.Runtime.Errors      as Errs

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
class ( DBIdentity a
      , DBStorageOps a
      , MonadError Errs.RuntimeErr m
      ) => DBStorage m a where
  -- | Execute an update, reporting the list of IDs that were affected due to it.
  dbUpdate :: DBUpdate a -> m [DBId a]

  -- | Execute a select, returning the rows that were matched by the query.
  dbSelect :: DBSelect a -> m [a]

-- | Perform a DBUpdate and get all the affected entities. 
gettingAffected
  :: forall a m
   . DBStorage m a
  => (DBId a -> DBSelect a)
  -> DBUpdate a
  -> m [a]
gettingAffected mkSelect = dbUpdate >=> concatMapM (dbSelect . mkSelect)

-- | Perform a DBUpdate and get the first of the affected entities. 
gettingAffectedFirstMaybe
  :: forall a m
   . DBStorage m a
  => (DBId a -> DBSelect a)
  -> DBUpdate a
  -> m (Maybe a)
gettingAffectedFirstMaybe mkSelect =
  gettingAffected mkSelect >=> pure . headMay

-- | Perform a DBUpdate and get the first of the affected entities: but throw errors when no resources can be retrieved. 
gettingAffectedFirstErr
  :: forall a m
   . (DBStorage m a, MonadError Errs.RuntimeErr m, Typeable a)
  => (DBId a -> DBSelect a)
  -> DBUpdate a
  -> m a
gettingAffectedFirstErr mkSelect =
  gettingAffectedFirstMaybe mkSelect >=> maybe resourceNotFound pure
 where
  resourceNotFound =
    Errs.throwError'
      . ResourceNotFound (typeRep $ Proxy @a)
      $ "No values retrieved after update."

data StorageErr = ResourceNotFound TypeRep Text
  deriving Show

instance Errs.IsRuntimeErr StorageErr where
  errCode = errCode' . \case
    ResourceNotFound tyRep _ -> prefix <> "NOT_FOUND"
     where
      prefix =
        ( Errs.ErrCode
        . pure
        . T.toUpper
        . T.pack
        . Typeable.tyConName
        . Typeable.typeRepTyCon
        $ tyRep
        )
    where errCode' = mappend "ERR.STORAGE"

  httpStatus = \case
    ResourceNotFound{} -> Status.notFound404

  userMessage = Just . \case
    ResourceNotFound _ msg -> msg
