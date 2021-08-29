{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , UndecidableInstances
#-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts
#-}
{-# LANGUAGE
    DerivingStrategies
  , DeriveAnyClass
  , DerivingVia 
  , DeriveGeneric 
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
#-}

module Prototype.Types.Secret
  ( Secret(..)
  , SecretExp(..)
  , exposeSecret
  ) where

import           Control.Lens
import           Data.Aeson
import qualified GHC.Show                      as Show
import qualified GHC.TypeLits                  as TL
import           Servant.API                    ( FromHttpApiData )

-- | Exposure level of the secret.
data SecretExp = ToJSONExp
  deriving (Eq, Show)

-- | Constraint: result in type errors when not met, empty otherwise.
type family HasExp (exp :: SecretExp) (exps :: [SecretExp]) :: Constraint where
  HasExp e '[] = TL.TypeError ('TL.Text "No exposure of type: " 'TL.:<>: 'TL.ShowType e)
  HasExp e ( e ': rest) = ()
  HasExp e ( _e ': rest) = HasExp e rest

{- | A Secret that can hold a value without accidentally revelaing it via Show.

A secret can have varying levels of exposure, and this can be indicated in the type-level list
of exposures.

This is made type-safe on purpose: this forces the user to provide exposures in type-signatures.

Failures to meet these will result in compile time errors, not runtime issues.

Example:

@ 
data Credentials = Credentials
  { username :: Namespace
  , password :: Secret '[] Text -- ^ Password, as secret: no exposure on ToJSONExp!
  }
  deriving (Eq, Show, Generic)

instance ToJSON Credentials
@

Will result in an type-error at compile time:

@
    • No exposure of type: 'ToJSONExp
    • In the expression:
        aeson-1.4.6.0:Data.Aeson.Types.ToJSON.$dmtoJSON @(Credentials)
      In an equation for ‘aeson-1.4.6.0:Data.Aeson.Types.ToJSON.toJSON’:
          aeson-1.4.6.0:Data.Aeson.Types.ToJSON.toJSON
            = aeson-1.4.6.0:Data.Aeson.Types.ToJSON.$dmtoJSON @(Credentials)
      In the instance declaration for ‘ToJSON Credentials’
    |
144 | instance ToJSON Credentials
    |          ^^^^^^^^^^^^^^^^^^

@
-}
newtype Secret (exp :: [SecretExp]) s = Secret { _exposeSecret :: s }
                 deriving ( Eq
                          , Read
                          , FromJSON
                          , FromHttpApiData
                          , Hashable
                          , Ord
                          , IsString
                          ) via s

makeLenses ''Secret

-- | An instance that only reveals the type-information of the value held by a secret. 
instance Typeable s => Show (Secret exp s) where
  show (Secret (_ :: s)) = "Secret :: " <> show (typeRep (Proxy @s))

-- | Secrets that can be exposed via ToJSON 
deriving via s instance (ToJSON s, HasExp 'ToJSONExp exps) => ToJSON (Secret exps s)
