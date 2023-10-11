module UnitTest.Main where

import           Plutus.Model
import           Test.Tasty
import qualified UnitTest.BasicPolicy    as BP
import qualified UnitTest.BasicValidator as BV

testWithLog :: Bool
testWithLog = False

main :: IO ()
main = do
  defaultMain $ do
    unitTests testWithLog

unitTests :: Bool -> TestTree
unitTests withLog = testGroup "Unit Test"
  [
    BP.mintingToken withLog defaultBabbage
  , BV.basicValidatorTest withLog defaultBabbage
  ]
