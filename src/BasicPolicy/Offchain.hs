{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module BasicPolicy.Offchain where

import           Ledger                         hiding (Value)
import           Ledger.Tx.Constraints          as Constraints
import           Plutus.Script.Utils.V2.Scripts as PSU.V2
import           Plutus.Script.Utils.Value      as Value

import           BasicPolicy.Onchain

mintBasicPolicy :: forall a. PubKeyHash -> TokenName -> Integer -> (Value, CurrencySymbol, ScriptLookups a)
mintBasicPolicy pkh tn q = (val, cs, lookups)
  where
    policy = policyV2 pkh
    cs = PSU.V2.scriptCurrencySymbol policy
    val = Value.singleton cs tn q
    lookups = plutusV2MintingPolicy policy
