{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- PlutusV2
module UnitTest.Utils.MockBasicValidator where

import qualified Ledger                                          (Address,
                                                                  PubKeyHash)
import           Plutus.Script.Utils.Typed                       (TypedValidator,
                                                                  ValidatorTypes,
                                                                  mkUntypedValidator,
                                                                  validatorAddress,
                                                                  validatorScript)
import           Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType,
                                                                  RedeemerType)
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators (mkTypedValidatorParam)
import           Plutus.V2.Ledger.Api
-- import Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude                                hiding
                                                                 (Semigroup (..),
                                                                  unless)

import           Types

{-# INLINABLE mkValidator #-}
mkValidator :: Ledger.PubKeyHash -> BuiltinData -> ValidatorRedeemer -> ScriptContext -> Bool
mkValidator _ _ _ _ = True

{-- Boilerplate --}
data BasicValidatorTypes
instance ValidatorTypes BasicValidatorTypes where
  type DatumType BasicValidatorTypes = BuiltinData
  type RedeemerType BasicValidatorTypes = ValidatorRedeemer

typedValidator :: Ledger.PubKeyHash -> TypedValidator BasicValidatorTypes
typedValidator = go
  where
    go =
      mkTypedValidatorParam @BasicValidatorTypes
        $$(PlutusTx.compile [||mkValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: Ledger.PubKeyHash -> Validator
validator = validatorScript . typedValidator

scriptAddress :: Ledger.PubKeyHash -> Ledger.Address
scriptAddress = validatorAddress . typedValidator
