{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-} 

module Common.Nervos where
 
import qualified Data.Text as T

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Basement.Numerical.Number as BNN
import Basement.Types.Word128 hiding ((-), (*))

import Common.Bridge
import Bridge.Utils
import Data.Aeson
import Data.Aeson.TH 
import qualified Data.Text.Lazy as LT
import Data.Maybe 

data Cell = Cell
  { cell_capacity :: T.Text
  , cell_lock :: Script
  , cell_type :: Maybe Script
  }
  deriving (Eq, Show)

-- TODO(galen): We also need the CKB hash
-- and ideally the time from this call 
data CkbTxInfo = CkbTxInfo
  { txInfo_transaction :: Tx
  , txInfo_tx_status :: CkbTxStatus 
  }
  deriving (Eq, Show)

-- TODO(galen): Should I add more validation on the block hash?
-- TODO(galen): Should we track why a transaction got rejected?
data CkbTxStatus = CkbTxStatus
  { txStatus_block_hash :: Maybe T.Text
  , txStatus_status :: StatusType
  }
  deriving (Eq, Show)

data StatusType = Pending
                | Proposed
                | Committed
                | Unknown
                | Rejected
                -- TODO(galen): what should we do here if rejected?
                deriving (Eq, Show) 
-- "pending" | "proposed" | "committed" | "unknown" | "rejected"

instance FromJSON StatusType where
  parseJSON = withText "status" $ \case
    "pending" -> pure Pending
    "proposed" -> pure Proposed
    "committed" -> pure Committed
    "unknown" -> pure Unknown
    "rejected" -> pure Rejected

instance ToJSON StatusType where
  toJSON s = String . T.toLower . T.pack . show $ s 

data Tx = Tx
  { tx_outputs :: [Cell]
  , tx_outputs_data :: [T.Text]
  , tx_hash :: CkbTxHash
  }
  deriving (Eq, Show)

newtype CkbTxHash = CkbTxHash { unCkbTxHash :: T.Text } deriving (Eq, Show)

instance FromJSON CkbTxHash where
  parseJSON = withText "hash" $ \x -> pure . CkbTxHash $ x 
    
instance ToJSON CkbTxHash where
  toJSON (CkbTxHash hash) = String hash
  


data MintTx =
  MintTx { assocLockTxHash :: AdaTxHash
         , mintTo :: Script
         , mintAmount :: Integer
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
 
deriveJSON (scrubPrefix "script_") ''Script
deriveJSON (scrubPrefix "outPoint_") ''OutPoint
deriveJSON (scrubPrefix "searchKey_") ''SearchKey
deriveJSON (scrubPrefix "searchResults_") ''SearchResults
deriveJSON (scrubPrefix "txRecord_") ''TxRecord
deriveJSON (scrubPrefix "cellDep_") ''CellDep
deriveJSON (scrubPrefix "tx_") ''Tx
deriveJSON (scrubPrefix "txInfo_") ''CkbTxInfo
deriveJSON (scrubPrefix "cell_") ''Cell
deriveJSON (scrubPrefix "txStatus_") ''CkbTxStatus






-- | Helper function to pull mint information related to a script from a transaction
getMints :: Script -> Tx -> [MintTx]
getMints script (Tx cells outputs _) = cs'
  where
    cs = zip cells outputs

    cs' :: [MintTx]
    cs' =
      catMaybes
      $ fmap (\(c, o) ->
                let
                  -- a single hex char represents a nibble (half a byte)
                  nibblesToSplitAt = 16 * 2;
                  (mintAmt, txHash) = T.splitAt nibblesToSplitAt o
                in MintTx (AdaTxHash txHash) (cell_lock c)
                    . BNN.toInteger . unSUDTAmount <$> fromHexUtf8 (rejig mintAmt)) cs


------------------All from SUDT.hs
 
-- TODO what is this type
newtype SUDTAmount =
  SUDTAmount { unSUDTAmount :: Word128 }
  deriving (Eq, Show, Num)

instance Binary SUDTAmount where
  put (SUDTAmount (Word128 ho lo) ) = do
    putWord64le lo
    putWord64le ho

  get =
    (\lo ho -> SUDTAmount $ Word128 ho lo) <$> getWord64le <*> getWord64le

-- TODO: not this
rejig :: T.Text -> T.Text
rejig t
  | t == "0x" = t
  | lenStr < ideallen = stripped <> (LT.toStrict $ LT.take (fromIntegral $ toInteger $ ideallen - lenStr) (LT.repeat '0'))
  | otherwise = stripped
  where
    lenStr = T.length stripped
    stripped = T.drop 2 t
    ideallen = T.length "e8030000000000000000000000000000"


-----------------------------------
