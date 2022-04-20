{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Description: Common bridge types and configuration
-}
module Common.Bridge where

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.TH

type Address = T.Text

newtype CKBAddress =
  CKBAddress { unCKBAddress :: T.Text }
  deriving (Eq, Show)

data CardanoBridgeMetadata = CardanoBridgeMetadata
   { mintToAddress :: !CKBAddress
   }
   deriving (Eq, Show)

testContractAddress :: Address
testContractAddress =
  "addr_test1qqvyvv446w768jj42wxrh99mpmk5kd2qppst0yma8qesllldkdcxe8fngwj6m2f9uk5k8unf94tzzryz7kujnnew29xse6rxsu"

deriveJSON defaultOptions ''CardanoBridgeMetadata
deriveJSON defaultOptions ''CKBAddress
