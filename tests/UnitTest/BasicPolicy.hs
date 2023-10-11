{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module UnitTest.BasicPolicy where

import           Plutus.Model
import           Plutus.Script.Utils.Value
import           Plutus.V2.Ledger.Api
import           Prelude
import           Test.Tasty

import qualified BasicPolicy.Onchain       as Token
import           Types

main :: IO ()
main = do
  defaultMain $ mintingToken True defaultBabbage

tokenScript :: PubKeyHash -> TypedPolicy PolicyRedeemer
tokenScript = TypedPolicy . toV2 . Token.policyV2

setupUsers :: Run [PubKeyHash]
setupUsers = do
  u1 <- newUser $ adaValue 2_000
  u2 <- newUser $ adaValue 2_000
  return [u1, u2]

data BPTestCase = BPCorrSig | BPCorrRed | BPBothAbsent
  deriving Eq

mintingToken :: Bool -> MockConfig -> TestTree
mintingToken testWithLog cfg = do
  testGroup
    "Testing minting token"
    [ good "Success minting with owner signature" $ mintingTest BPCorrSig
    , good "Success minting with correct redeemer" $ mintingTest BPCorrRed
    , bad  "Fail when owner signature and correct redeemer both absent" $ mintingTest BPBothAbsent
    ]
  where
    bad msg = good msg . mustFail
    good = let testingMethod = if testWithLog then testNoErrorsTrace else testNoErrors in
      testingMethod (adaValue 10_000_000) cfg

mintingTest :: BPTestCase -> Run ()
mintingTest testCase = do
  [u1, u2] <- setupUsers
  let minter = if testCase == BPCorrSig then u1 else u2
  mintTokenTx u1 minter testCase
  waitNSlots 5

mintTokenTx :: PubKeyHash -> PubKeyHash -> BPTestCase -> Run ()
mintTokenTx owner pkh testCase = do
    let mintVal = singleton (scriptCurrencySymbol $ tokenScript owner) "TestToken" 1
        red = if testCase == BPCorrRed then PolRedeemerTwo else PolRedeemerOne
    submitTx pkh $ tx mintVal red
  where
    tx mintVal red = mconcat
      [ mintValue (tokenScript owner) red mintVal
      , payToKey pkh mintVal
      ]
