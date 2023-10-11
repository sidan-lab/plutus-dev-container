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
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-context #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

-- PlutusV2
module BasicValidator.Onchain where

import qualified Ledger                    (PubKeyHash, scriptHashAddress,
                                            validatorHash)
import           Ledger.Tx
import           Plutus.Script.Utils.Typed (mkUntypedValidator)
import           Plutus.V2.Ledger.Api
-- import Plutus.V2.Ledger.Contexts
import qualified Plutonomy
import           PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)

import           Common
import           Ledger                    (PaymentPubKeyHash (PaymentPubKeyHash))
import           Types

{-# INLINABLE mkValidator #-}
mkValidator :: Ledger.PubKeyHash -> BuiltinData -> ValidatorRedeemer -> ScriptContext -> Bool
mkValidator pkh _ red ctx = case red of
  ValRedeemerOne -> contractSignerCheck ctx $ PaymentPubKeyHash pkh
  ValRedeemerTwo -> True

{-- Boilerplate --}
-- data BasicValidatorTypes
-- instance ValidatorTypes BasicValidatorTypes where
--   type DatumType BasicValidatorTypes = BuiltinData
--   type RedeemerType BasicValidatorTypes = ValidatorRedeemer

-- typedValidator :: Ledger.PubKeyHash -> TypedValidator BasicValidatorTypes
-- typedValidator = go
--   where
--     go =
--       mkTypedValidatorParam @BasicValidatorTypes
--         $$(PlutusTx.compile [||mkValidator||])
--         $$(PlutusTx.compile [||wrap||])
--     wrap = mkUntypedValidator

-- validator :: Ledger.PubKeyHash -> Validator
-- validator = validatorScript . typedValidator

-- scriptAddress :: Ledger.PubKeyHash -> Ledger.Address
-- scriptAddress = validatorAddress . typedValidator

-- validatorHash :: Ledger.PubKeyHash -> ValidatorHash
-- validatorHash = validatorHash . typedValidator

validator :: Ledger.PubKeyHash -> Validator
validator pkh = Plutonomy.optimizeUPLC $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript
  ($$(PlutusTx.compile [||mkUntypedValidator . mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)

versionedValidator :: Ledger.PubKeyHash -> Versioned Validator
versionedValidator pkh = Versioned {unversioned = validator pkh, version = Ledger.Tx.PlutusV2}

valHash :: Ledger.PubKeyHash -> ValidatorHash
valHash = Ledger.validatorHash . versionedValidator

scriptAddress :: Ledger.PubKeyHash -> Address
scriptAddress = Ledger.scriptHashAddress . valHash
