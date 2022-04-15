{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Description: Common bridge types and configuration for Cardano
-}

module Common.Cardano where

import qualified Data.Text as T
import Common.Bridge
import Data.Aeson
import Data.Aeson.TH 


data CardanoBridgeMetadata = CardanoBridgeMetadata
   { mintToAddress :: CKBAddress
   }
   deriving (Eq, Show)


testContractAddress :: T.Text
testContractAddress =
  "addr_test1qqvyvv446w768jj42wxrh99mpmk5kd2qppst0yma8qesllldkdcxe8fngwj6m2f9uk5k8unf94tzzryz7kujnnew29xse6rxsu"

deriveJSON defaultOptions ''CardanoBridgeMetadata
