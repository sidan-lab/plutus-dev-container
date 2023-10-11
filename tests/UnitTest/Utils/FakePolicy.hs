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

module UnitTest.Utils.FakePolicy where

import           Plutus.Model
import qualified Plutus.Script.Utils.Typed      as PSU.V2
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import           Plutus.Script.Utils.Value      (CurrencySymbol)
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude               hiding (Semigroup (..), unless)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinByteString -> () -> PlutusV2.ScriptContext -> Bool
mkPolicy _ _ _ = True

policyV2 :: BuiltinByteString -> PSU.V2.MintingPolicy
policyV2 oref = PlutusV2.mkMintingPolicyScript $ $$(PlutusTx.compile [|| PSU.V2.mkUntypedMintingPolicy . mkPolicy ||])
  `PlutusTx.applyCode`
  PlutusTx.liftCode oref

fakeCS :: BuiltinByteString -> CurrencySymbol
fakeCS = PSU.V2.scriptCurrencySymbol . policyV2

fakeTP :: BuiltinByteString -> TypedPolicy ()
fakeTP = TypedPolicy . toV2 . policyV2
