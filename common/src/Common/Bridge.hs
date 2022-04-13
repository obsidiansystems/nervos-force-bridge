{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-} 

{- |
Description: Common bridge types and configuration
-}
module Common.Bridge where

import qualified Data.Text as T

-- import qualified Bridge.Nervos.Types as CKB 
import Bridge.Utils
import Data.Aeson
import Data.Aeson.TH

newtype AdaTxHash =
  AdaTxHash { unAdaTxHash :: T.Text }
  deriving (Eq, Show)

data MintTx =
  MintTx { lockTxHash :: AdaTxHash
         , mintTo :: Script
         , mintAmount :: Integer
         }
  deriving (Eq, Show)

newtype CKBAddress =
  CKBAddress { unCKBAddress :: T.Text }
  deriving (Eq, Show)

data CardanoBridgeMetadata = CardanoBridgeMetadata
   { mintToAddress :: CKBAddress
   }
   deriving (Eq, Show)

data DeployedScript =
  DeployedScript { deployedScriptScript :: Script
                 , deployedScriptDep :: CellDep
                 }
  deriving (Eq)

data ScriptType =
  Lock | Type

data SearchKey = SearchKey
  { searchKey_script :: Script
  , searchKey_script_type :: ScriptType
  -- TODO Do we want a filter?
  }

data SearchResults = SearchResults
  { searchResults_last_cursor :: T.Text
  , searchResults_objects :: [TxRecord]
  }
  deriving (Eq, Show)

data TxRecord = TxRecord
  { txRecord_tx_hash :: T.Text
  }
  deriving (Eq, Show)

data Script = Script
  { script_code_hash :: T.Text
  , script_hash_type :: HashType
  , script_args :: T.Text
  }
  deriving (Eq, Show)

data Order = Asc | Desc

data HashType =
  HashTypeType | HashTypeData
  deriving (Eq, Show)

data CellDep = CellDep
  { cellDep_out_point :: OutPoint
  , cellDep_dep_type :: DepType
  }
  deriving (Eq, Show)

data OutPoint = OutPoint
  {
    outPoint_tx_hash :: T.Text
  , outPoint_index :: T.Text
  }
  deriving (Eq, Show)

data DepType =
  Group | Code
  deriving (Eq, Show)

instance ToJSON ScriptType where
   toJSON = \case
     Lock -> String "lock"
     Type -> String "type"

instance FromJSON ScriptType where
  parseJSON = withText "ScriptType" $ \case
    "lock" -> pure Lock
    "type" -> pure Type
    t -> fail $ "Invalid Script Type: " <> T.unpack t

instance ToJSON Order where
   toJSON = \case
     Asc -> String "asc"
     Desc -> String "desc"

instance FromJSON Order where
  parseJSON = withText "Order" $ \case
    "asc" -> pure Asc
    "desc" -> pure Desc
    t -> fail $ "Invalid order: " <> T.unpack t

instance ToJSON DepType where
  toJSON dt = String $ case dt of
    Group -> "dep_group"
    Code -> "code"

instance FromJSON DepType where
  parseJSON = withText "DepType" $ \case
    "dep_group" -> pure Group
    "code" -> pure Code
    _ -> fail "Not a valid DepType"

testContractAddress :: T.Text
testContractAddress =
  "addr_test1qqvyvv446w768jj42wxrh99mpmk5kd2qppst0yma8qesllldkdcxe8fngwj6m2f9uk5k8unf94tzzryz7kujnnew29xse6rxsu"

deployedSUDT :: Script
deployedSUDT = Script
  "0xc5e5dcf215925f7ef4dfaf5f4b4f105bc321c02776d6e7d52a1db3fcd9d011a4"
  HashTypeType
  "0x15cec0cbd70ba5a93d6e0620893cfe6159c9b3c7ce25dc8541f567fc19f03855"

deployedSUDTDep :: CellDep
deployedSUDTDep = CellDep
  (OutPoint "0xe12877ebd2c3c364dc46c5c992bcfaf4fee33fa13eebdf82c591fc9825aab769" "0x0")
  Code

deployedCKBScript :: DeployedScript
deployedCKBScript = DeployedScript deployedSUDT deployedSUDTDep 

instance ToJSON HashType where
  toJSON dt = String $ case dt of
    HashTypeType -> "type"
    HashTypeData -> "data"

instance FromJSON HashType where
  parseJSON = withText "HashType" $ \case
    "type" -> pure HashTypeType
    "data" -> pure HashTypeData
    _ -> fail "Not a valid HashType"

deriveJSON defaultOptions ''Script
deriveJSON defaultOptions ''CardanoBridgeMetadata
deriveJSON defaultOptions ''CKBAddress
deriveJSON (scrubPrefix "outPoint_") ''OutPoint
deriveJSON (scrubPrefix "searchKey_") ''SearchKey
deriveJSON (scrubPrefix "searchResults_") ''SearchResults
deriveJSON (scrubPrefix "txRecord_") ''TxRecord
deriveJSON (scrubPrefix "cellDep_") ''CellDep





