{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module BasicValidator.Offchain where

import           Data.Functor
import           Data.Monoid             (Last (..))
import           Data.Text               (Text)
import           Data.Void               (Void)
import           Ledger                  hiding (Value)
import           Ledger.Tx.Constraints   as Constraints
import           Plutus.Contract         as Contract
import           Plutus.Script.Utils.Ada as Ada
import           Plutus.V2.Ledger.Api
-- import PlutusTx.Prelude (find)

import qualified BasicPolicy.Offchain    as Token.Off
import qualified BasicValidator.Onchain  as Validator.On
import           Types

type ChainInfo = PubKeyHash

type BasicSchema =
      Endpoint "mintToken" ()

mintToken :: forall s. Contract (Last ChainInfo) s Text ()
mintToken = do
    ownPpkh <- Contract.ownFirstPaymentPubKeyHash
    let ownPkh = unPaymentPubKeyHash ownPpkh
        valHash = Validator.On.valHash ownPkh
        (tokenVal, _, mintLookups) = Token.Off.mintBasicPolicy ownPkh "test token" 1
        lookups = mintLookups <> Constraints.plutusV2OtherScript (Validator.On.validator ownPkh)
        mintTx = Constraints.mustMintValueWithRedeemer (Redeemer $ toBuiltinData PolRedeemerTwo) tokenVal <>
                 Constraints.mustPayToOtherScriptWithInlineDatum valHash (Datum $ toBuiltinData ()) (tokenVal <> Ada.lovelaceValueOf 5000000)
    tell $ Last $ Just ownPkh
    ledgerTx <- submitTxConstraintsWith @Void lookups mintTx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

endpoints :: Contract (Last ChainInfo) BasicSchema Text ()
endpoints = awaitPromise mintToken' >> endpoints
  where
    mintToken' = endpoint @"mintToken" $ const mintToken

