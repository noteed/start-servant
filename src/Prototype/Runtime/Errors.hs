{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{- |
Module: Prototype.Runtime.Errors
Description: Custom errors and support for throwing these errors in some monad.

We introduce RuntimeErr's that can be used to wrap either errors that are instances of `IsRuntimeErr`; or exceptions that are otherwise not handled.
These errors then get mapped to Servant errors.

Consider the following example:

@
data AuthErr = LoginFailed Text | PermissionDenied Text
             deriving Show

instance IsRuntimeErr AuthErr where

  httpStatus = \\case
    LoginFailed{} -> HTTP.unauthorized401
    PermissionDenied{} -> HTTP.forbidden403

  userMessage = Just . \\case
    LoginFailed msg -> msg
    PermissionDenied msg -> msg

@

-}
module Prototype.Runtime.Errors
  ( RuntimeErr(..)
  , IsRuntimeErr(..)
  , asServantError
  ) where

import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Network.HTTP.Types
import qualified Network.HTTP.Types            as HTTP
import           Servant.Server                 ( ServerError(..) )

-- brittany-disable-next-binding 
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
  -- TODO: also add language code as a parameter for localisation (later)
  userMessage :: e -> Maybe Text

  displayErr :: e -> Text
  default displayErr :: Show e => e -> Text
  displayErr e = T.unwords [ "httpStatus =", show $ httpStatus e
                           , "userMessage =", fromMaybe "" (userMessage e)
                           ]

  -- | Header information to supply for returning errors over HTTP.
  httpHeaders :: e -> [Header]
  httpHeaders _ = mempty

instance IsRuntimeErr RuntimeErr where

  knownErr   = identity

  httpStatus = \case
    KnownErr e         -> httpStatus e
    RuntimeException{} -> HTTP.internalServerError500

  throwError' = throwError

  userMessage = \case
    KnownErr e         -> userMessage e
    RuntimeException{} -> Just "Internal exception, we're sorry about that."

  displayErr = \case
    KnownErr e -> displayErr e
    RuntimeException e ->
      T.unwords ["RuntimeException", show e, T.pack $ displayException e]

-- | Map out a known error to a `ServerError` (from Servant)
asServantError :: IsRuntimeErr e => e -> ServerError
asServantError e = ServerError
  { errReasonPhrase = T.unpack . TE.decodeUtf8 $ statusMessage
  , errHeaders      = httpHeaders e
  , ..
  }
 where
  Status errHTTPCode statusMessage = httpStatus e
  errBody =
    maybe "No known reason." (BSL.fromStrict . TE.encodeUtf8) $ userMessage e

