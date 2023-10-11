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

module BasicPolicy.Onchain where

import qualified Plutonomy
import qualified Plutus.Script.Utils.Typed as PSU.V2
import qualified Plutus.V2.Ledger.Api      as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup (..), unless)

import           Common
import           Ledger
import           Types

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> PolicyRedeemer -> PlutusV2.ScriptContext -> Bool
mkPolicy pkh red ctx = case red of
  PolRedeemerOne -> contractSignerCheck ctx $ PaymentPubKeyHash pkh
  PolRedeemerTwo -> True

-- policyV2 :: PubKeyHash -> PSU.V2.MintingPolicy
-- policyV2 pkh = PlutusV2.mkMintingPolicyScript $ $$(PlutusTx.compile [|| PSU.V2.mkUntypedMintingPolicy . mkPolicy ||])
--   `PlutusTx.applyCode`
--   PlutusTx.liftCode oref

policyV2 :: PubKeyHash -> PlutusV2.MintingPolicy
policyV2 pkh = Plutonomy.optimizeUPLC $ Plutonomy.mintingPolicyToPlutus $ Plutonomy.mkMintingPolicyScript
  ($$(PlutusTx.compile [||PSU.V2.mkUntypedMintingPolicy . mkPolicy||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh)
