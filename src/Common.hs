{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Ledger
import           Ledger.Address             (PaymentPubKeyHash (..))
import qualified Plutus.Script.Utils.Ada    as Ada
import           Plutus.Script.Utils.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified PlutusTx.Builtins.Internal as PBI
import           PlutusTx.Prelude

{--------------  Basic Utils --------------}
{-# INLINABLE info #-}
info :: ScriptContext -> TxInfo
info = scriptContextTxInfo

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE hasUTxO #-}
hasUTxO :: TxOutRef -> ScriptContext -> Bool
hasUTxO utxo ctx = any (\i -> txInInfoOutRef i == utxo) $ txInfoInputs (info ctx)

{--------------  Signers  --------------}
{-# INLINABLE contractSignerCheck #-}
contractSignerCheck :: ScriptContext -> PaymentPubKeyHash -> Bool
contractSignerCheck ctx pkh = txSignedBy (info ctx) (unPaymentPubKeyHash pkh)

{-# INLINABLE getOnlySigner #-}
getOnlySigner :: ScriptContext -> Maybe PaymentPubKeyHash
getOnlySigner ctx = case txInfoSignatories (info ctx) of
  [pkh] -> Just (PaymentPubKeyHash pkh)
  _     -> Nothing

{--------------  Input Outputs  --------------}

{-# INLINABLE minUTxO #-}
minUTxO :: Integer
minUTxO = 2000000

{-# INLINABLE countHashes #-}
countHashes :: ScriptContext -> Integer
countHashes ctx = go $ txInfoInputs $ info ctx
  where
    go [] = 0
    go (i:is) = case Ledger.toValidatorHash $ txOutAddress $ txInInfoResolved i of
      Just _  -> 1 + go is
      Nothing -> go is

{-# INLINABLE getOwnInputs #-}
getOwnInputs :: ScriptContext -> [TxOut]
getOwnInputs ctx | Just TxInInfo{txInInfoResolved=TxOut{txOutAddress}} <- findOwnInput ctx = map txInInfoResolved $ filter (f txOutAddress) (txInfoInputs $ scriptContextTxInfo ctx)
    where
        f addr (TxInInfo _ TxOut{txOutAddress=otherAddress}) = addr == otherAddress
getOwnInputs _ = traceError "Lf" -- "Can't get any continuing outputs"

{-# INLINABLE ownOutput #-}
ownOutput :: ScriptContext -> TxOut
ownOutput ctx = case getContinuingOutputs ctx of
  [o] -> o
  _   -> traceError "expected exactly one output"

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx = case getOwnInputs ctx of
  [o] -> o
  _   -> traceError "expected exactly one input"

{-# INLINABLE inputOutputOnlyContainToken #-}
inputOutputOnlyContainToken :: TxOut -> TxOut -> AssetClass -> Bool
inputOutputOnlyContainToken i o ac = let
  !iVal = txOutValue i
  !oVal = txOutValue o
  in
  assetClassValueOf iVal ac == 1 && assetClassValueOf oVal ac == 1 &&
  length (flattenValue iVal) <= 2 && length (flattenValue oVal) <= 2

{-# INLINABLE adaPaidTo #-}
adaPaidTo :: ScriptContext -> PaymentPubKeyHash -> Integer -> Bool
adaPaidTo ctx pkh amount = amount <= lovelaces (valuePaidTo (info ctx) $ unPaymentPubKeyHash pkh)

{-# INLINABLE onlyTokenMinted #-}
onlyTokenMinted :: ScriptContext -> AssetClass -> Integer -> Bool
onlyTokenMinted ctx t q = txInfoMint (info ctx) == assetClassValue t q

{-# INLINABLE valueToAddress #-}
valueToAddress :: ScriptContext -> Address -> Value
valueToAddress ctx addr = go $ txInfoOutputs $ info ctx
  where
    go [] = mempty
    go (o:os)
      | txOutAddress o == addr = txOutValue o <> go os
      | otherwise = go os

{--------------  Datum  --------------}
{-# INLINABLE getTxOutsToAddress #-}
getTxOutsToAddress :: ScriptContext -> Address -> [TxOut]
getTxOutsToAddress ctx addr = getTxOut (txInfoOutputs $ info ctx)
  where
    getTxOut [] = []
    getTxOut (t:ts)
      | txOutAddress t == addr = t: getTxOut ts
      | otherwise = getTxOut ts

{-# INLINABLE getTxInsFromAddress #-}
getTxInsFromAddress :: ScriptContext -> Address -> [TxOut]
getTxInsFromAddress ctx addr = getTxOut (txInfoInputs $ info ctx)
  where
    getTxOut [] = []
    getTxOut (t:ts)
      | txOutAddress (txInInfoResolved t) == addr = txInInfoResolved t : getTxOut ts
      | otherwise = getTxOut ts

{-# INLINABLE convertIntegerToByte #-}
convertIntegerToByte :: Integer -> BuiltinByteString
convertIntegerToByte int = go int (getNumberDigit int)
  where
    go newInt digit
      | digit == 1 = PBI.consByteString (48 + newInt) PBI.emptyByteString
      | otherwise = PBI.consByteString (48 + newInt `PBI.divideInteger` digit) (go (newInt `PBI.remainderInteger` digit) (digit `PBI.divideInteger` 10))

{-# INLINABLE getNumberDigit #-}
getNumberDigit :: Integer -> Integer
getNumberDigit int = go int 1
  where
    go newInt digit
      | newInt < 10 = digit
      | otherwise = go (newInt `PBI.divideInteger` 10) (digit `PBI.multiplyInteger` 10)

{-# INLINABLE getInlineDatum #-}
getInlineDatum :: TxOut -> Maybe BuiltinData
getInlineDatum tx = case txOutDatum tx of
  NoOutputDatum             -> Nothing
  (OutputDatumHash _)       -> Nothing
  (OutputDatum (Datum dat)) -> Just dat

{-# INLINEABLE getTxOutDatumHash #-}
getTxOutDatumHash :: TxOut -> Maybe DatumHash
getTxOutDatumHash txOut = case txOutDatum txOut of
  OutputDatumHash hash -> Just hash
  _                    -> Nothing

{--------------  Time  --------------}
beforeMaturity :: ScriptContext -> POSIXTime -> Bool
beforeMaturity ctx m = Ledger.contains (to m) $ txInfoValidRange $ info ctx

afterMaturity :: ScriptContext -> POSIXTime -> Bool
afterMaturity ctx m = Ledger.contains (from m) $ txInfoValidRange $ info ctx

{---------------- Reference UTxO Utils -----------------}
{-# INLINABLE valueElement #-}
valueElement :: Value -> CurrencySymbol -> Bool
valueElement v cs = cs `elem` symbols v

{-# INLINABLE getRefUTxO #-}
getRefUTxO :: ScriptContext -> CurrencySymbol -> Maybe TxOut
getRefUTxO ctx cs = case find (\x -> valueElement (txOutValue $ txInInfoResolved x) cs) (txInfoReferenceInputs $ info ctx) of
  Just i -> Just $ txInInfoResolved i
  _      -> Nothing

{-# INLINABLE refUTxOCorrect #-}
refUTxOCorrect :: CurrencySymbol -> TxOut -> Bool
refUTxOCorrect cs tx = cs `elem` symbols (txOutValue tx)

{-# INLINABLE getInputUTxO #-}
getInputUTxO :: ScriptContext -> CurrencySymbol -> Maybe TxOut
getInputUTxO ctx cs = case find (\x -> valueElement (txOutValue $ txInInfoResolved x) cs) (txInfoInputs $ info ctx) of
  Just i -> Just $ txInInfoResolved i
  _      -> Nothing

{-# INLINABLE getRefUTxOTxInInfo #-}
getRefUTxOTxInInfo :: ScriptContext -> CurrencySymbol -> Maybe TxInInfo
getRefUTxOTxInInfo ctx cs = case find (\x -> valueElement (txOutValue $ txInInfoResolved x) cs) (txInfoReferenceInputs $ info ctx) of
  Just i -> Just i
  _      -> Nothing

{-# INLINABLE getTxInsFromAddressWithTxOutRef #-}
getTxInsFromAddressWithTxOutRef :: ScriptContext -> Address -> [TxInInfo]
getTxInsFromAddressWithTxOutRef ctx addr = getTxOut (txInfoInputs $ info ctx)
  where
    getTxOut [] = []
    getTxOut (t:ts)
      | txOutAddress (txInInfoResolved t) == addr = t : getTxOut ts
      | otherwise = getTxOut ts
