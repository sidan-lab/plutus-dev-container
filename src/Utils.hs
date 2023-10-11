{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils where

import           Plutus.V2.Ledger.Api   as PlutusV2

{- Imports below for writing Validator and Minting Policy -}
import           Cardano.Api            as API
import           Cardano.Api.Shelley    (PlutusScript (..))
import           Codec.Serialise        (serialise)
import           Data.Aeson             (encode)
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Short  as SBS
import qualified Ledger                 as Plutus
import qualified PlutusTx

import qualified BasicPolicy.Onchain    as Policy
import qualified BasicValidator.Onchain as Validator


dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "scripts/unit.json" ()

writeValidator :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.unValidatorScript

writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy

{- write plutus -}
writeBasicPolicy :: PubKeyHash -> IO (Either (FileError ()) ())
writeBasicPolicy param = writeMintingPolicy "scripts/basic-policy.plutus" (Policy.policyV2 param)

writeBasicValidator :: PubKeyHash -> IO (Either (FileError ()) ())
writeBasicValidator param = writeValidator "scripts/basic-validator.plutus" (Validator.validator param)
