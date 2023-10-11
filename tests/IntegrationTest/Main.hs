{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Trace.Main where

import           Control.Monad           hiding (fmap)
import           Data.Default            (Default (..))
import qualified Data.Map                as Map
import           Ledger
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator   as Emulator

import qualified BasicValidator.Offchain as Validator


main :: IO ()
main = runMyTrace

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 5]]) def
  where
    v :: Ledger.Value
    v = Ledger.lovelaceValueOf 2_000_000_000

myTrace :: EmulatorTrace ()
myTrace = do
    let kw1 = knownWallet 1

    h1 <- activateContract kw1 Validator.endpoints (walletInstanceTag kw1)

    void $ Emulator.waitNSlots 5

    callEndpoint @"mintToken" h1 ()

    void $ Emulator.waitNSlots 15
