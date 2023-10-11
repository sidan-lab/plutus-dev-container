{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import qualified Prelude      as Pr

data ValidatorRedeemer = ValRedeemerOne | ValRedeemerTwo
   deriving (Pr.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ValidatorRedeemer [('ValRedeemerOne, 0), ('ValRedeemerTwo, 1)]
PlutusTx.makeLift ''ValidatorRedeemer

data PolicyRedeemer = PolRedeemerOne | PolRedeemerTwo
   deriving (Pr.Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''PolicyRedeemer [('PolRedeemerOne, 0), ('PolRedeemerTwo, 1)]
PlutusTx.makeLift ''PolicyRedeemer
