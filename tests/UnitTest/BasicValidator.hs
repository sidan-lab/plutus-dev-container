{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module UnitTest.BasicValidator where

import           Plutus.Model
import           Plutus.Script.Utils.Value
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty

import qualified BasicValidator.Onchain    as Validator
import           Types
import qualified UnitTest.Utils.FakePolicy as FP

main :: IO ()
main = do
  defaultMain $ do
    basicValidatorTest True defaultBabbage

basicValidatorScript :: PubKeyHash -> TypedValidator BuiltinData ValidatorRedeemer
basicValidatorScript = TypedValidator . toV2 . Validator.validator

mockToken :: Integer -> Value
mockToken = singleton (FP.fakeCS "MockToken") "MockToken"

setupUsers :: Run [PubKeyHash]
setupUsers = do
  u1 <- newUser $ adaValue 1_000_000 <> mockToken 1000
  u2 <- newUser $ adaValue 1_000_000 <> mockToken 1000
  return [u1, u2]

data BVTestCase = BVCorrSig | BVCorrRed | BVBothAbsent
  deriving Eq

basicValidatorTest :: Bool -> MockConfig -> TestTree
basicValidatorTest testWithLog cfg = do
  testGroup
    "Testing Basic Validator"
    [ good "Success unlocking with owner signature" $ validatorTest BVCorrSig
    , good "Success unlocking with correct redeemer" $ validatorTest BVCorrRed
    , bad  "Fail when owner signature and correct redeemer both absent" $ validatorTest BVBothAbsent
    ]
  where
    bad msg = good msg . mustFail
    good = let testingMethod = if testWithLog then testNoErrorsTrace else testNoErrors in
      testingMethod (adaValue 10_000_000 <> mockToken 2000) cfg

validatorTest :: BVTestCase -> Run ()
validatorTest testCase = do
  [u1, u2] <- setupUsers
  sendTokenTx u1
  waitNSlots 5

  let unlocker = if testCase == BVCorrSig then u1 else u2
  unlockTx u1 unlocker testCase
  waitNSlots 5

sendTokenTx :: PubKeyHash -> Run ()
sendTokenTx pkh = do
    let val = adaValue 2_000 <> mockToken 500
    usp <- spend pkh val
    submitTx pkh $ tx usp val
  where
    tx usp val = mconcat
      [ userSpend usp
      , payToScript (basicValidatorScript pkh) (InlineDatum $ toBuiltinData ()) val
      ]

unlockTx :: PubKeyHash -> PubKeyHash -> BVTestCase -> Run ()
unlockTx owner pkh testCase = do
    let red = if testCase == BVCorrRed then ValRedeemerTwo else ValRedeemerOne
    utxos <- utxoAt $ basicValidatorScript owner
    submitTx pkh $ tx (head utxos) red
  where
    tx (oref, o) red = mconcat
      [ spendScript (basicValidatorScript owner) oref red (toBuiltinData ())
      , payToKey pkh (txOutValue o)
      ]
