{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Prototype.Runtime.Errors
  ( RuntimeErr(..)
  , IsRuntimeErr(..)
  ) where

import qualified Network.HTTP.Types            as HTTP

-- | A generalised error
data RuntimeErr where
  -- | Capture known error types. 
  KnownErr ::IsRuntimeErr e => e -> RuntimeErr
  -- | Capture all exceptions 
  RuntimeException ::Exception e => e -> RuntimeErr

-- | TODO: add common properties of errors. 
class IsRuntimeErr e where

  -- | Construct a `RuntimeErr` from an instance value  
  knownErr :: e -> RuntimeErr
  knownErr = KnownErr

  -- | What should the error status be in HTTP-terms?
  httpStatus :: e -> HTTP.Status

  -- | Throw an error created with an instance value
  throwError' :: MonadError RuntimeErr m => e -> m a
  throwError' = throwError . knownErr

  -- | Informative user message
  -- TODO: also add langauge code as a parameter for localisation (later) 
  userMessage :: e -> Maybe Text

instance IsRuntimeErr RuntimeErr where

  knownErr = identity

  httpStatus = \case
    KnownErr e -> httpStatus e
    RuntimeException{} -> HTTP.internalServerError500 

  throwError' = throwError 

  userMessage = \case
    KnownErr e -> userMessage e
    RuntimeException{} -> Just "Internal exception, we're sorry about that."
