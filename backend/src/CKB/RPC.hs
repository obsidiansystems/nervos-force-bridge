{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-|
Description: CKB RPC calls and Providers
-}

module CKB.RPC where

import Data.Maybe

import Text.Read (readMaybe)
import qualified Basement.Numerical.Number as BNN

import Data.Traversable
-- import Prelude hiding (Integral)
-- import GHC.Real.Integral

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import System.IO.Unsafe
import Control.Exception

import Network.Web3.Provider
import Network.JsonRpc.TinyClient
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Aeson
import Data.Aeson.TH
import Data.HexString
-- import Basement.Types
import Basement.Types.Word128 hiding ((-))

import CKB.Utils

import Data.Map (Map)
import qualified Data.Map as Map

-- CARDANO STUFF

data OutPoint = OutPoint
  {
    outPoint_tx_hash :: T.Text
  , outPoint_index :: T.Text
  }
  deriving (Eq, Show)

data DepType =
  Group | Code
  deriving (Eq, Show)

instance ToJSON DepType where
  toJSON dt = String $ case dt of
    Group -> "dep_group"
    Code -> "code"

instance FromJSON DepType where
  parseJSON = withText "DepType" $ \case
    "dep_group" -> pure Group
    "code" -> pure Code
    _ -> fail "Not a valid DepType"

data CellDep = CellDep
  { cellDep_out_point :: OutPoint
  , cellDep_dep_type :: DepType
  }
  deriving (Eq, Show)

-- TODO(skylar): Steal this from cardano
data AssetType
  = Ada
  | AssetName T.Text
  deriving (Eq, Show, Read, Ord)

assetTypeFromText :: T.Text -> AssetType
assetTypeFromText "lovelace" = Ada
assetTypeFromText n = AssetName n

-- TODO: Should the map just be "value"
data CardanoOutput = CardanoOutput
  { co_address :: T.Text
  , co_amount :: Map AssetType Integer
  }
  deriving (Eq, Show)

-- Parser String -> Parser (Maybe Integer)

instance FromJSON CardanoOutput where
  parseJSON = withObject "CardanoOutput" $ \v -> do
    address <- v .: "address"
    lv <- v .: "amount"
    values <- fmap mconcat <$> for lv $ \e -> do
      assetType <- assetTypeFromText <$> (e .: "unit")
      mQuantity <- readMaybe <$> e .: "quantity"
      case mQuantity of
        Nothing -> fail "Not a valid integer"
        Just quantity -> do
          pure $ Map.singleton assetType quantity
    pure $ CardanoOutput address values

instance ToJSON CardanoOutput where
  toJSON _ = object [
                    ]

data TxOutputs = TxOutputs
  { txo_hash :: T.Text
  , txo_outputs :: [CardanoOutput]
  }
  deriving (Eq, Show)

-- NERVOS STUFF

-- TODO IMPORTANT What about the Tx hash
data MintTx =
  MintTx { mintTo :: Script
         , mintAmount :: Integer
         }
  deriving (Eq, Show)

data DecodeResult = DecodeResult
  { lock_script :: Script
  }
  deriving (Eq, Show)

data BlockTip = BlockTip
  { block_hash :: T.Text
  , block_number :: T.Text
  }
  deriving (Eq, Show)

data Script = Script
  { code_hash :: T.Text
  , hash_type :: T.Text -- Make an ADT
  , args :: T.Text
  }
  deriving (Eq, Show)

data ScriptType =
  Lock | Type

instance ToJSON ScriptType where
   toJSON = \case
     Lock -> String "lock"
     Type -> String "type"

instance FromJSON ScriptType where
  parseJSON = withText "ScriptType" $ \case
    "lock" -> pure Lock
    "type" -> pure Type
    t -> fail $ "Invalid Script Type: " <> T.unpack t

data SearchKey = SearchKey
  { script :: Script
  , script_type :: ScriptType
  -- TODO Do we want a filter?
  }

data Order = Asc | Desc

instance ToJSON Order where
   toJSON = \case
     Asc -> String "asc"
     Desc -> String "desc"

instance FromJSON Order where
  parseJSON = withText "Order" $ \case
    "asc" -> pure Asc
    "desc" -> pure Desc
    t -> fail $ "Invalid order: " <> T.unpack t

data TxRecord = TxRecord
  { tx_hash :: T.Text
  }
  deriving (Eq, Show)

data Cell = Cell
  { cell_capacity :: T.Text
  , cell_lock :: Script
  , cell_type :: Maybe Script
  }
  deriving (Eq, Show)

data TxInfo = TxInfo
  { transaction :: Tx
  }
  deriving (Eq, Show)

data Tx = Tx
  { outputs :: [Cell]
  , outputs_data :: [T.Text]
  }
  deriving (Eq, Show)

data SearchResults = SearchResults
  { last_cursor :: T.Text
  , objects :: [TxRecord]
  }
  deriving (Eq, Show)

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

-- TODO more elegant json handling and naming
deriveJSON (scrubPrefix "txo_") ''TxOutputs
deriveJSON (scrubPrefix "cell_") ''Cell
deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxInfo
deriveJSON defaultOptions ''DecodeResult

deriveJSON defaultOptions ''SearchResults
deriveJSON defaultOptions ''TxRecord
deriveJSON defaultOptions ''Script
deriveJSON defaultOptions ''SearchKey
deriveJSON defaultOptions ''BlockTip

-- TODO(skylar): These need to be in a config
ckbIndexerProvider :: Provider
ckbIndexerProvider =
  HttpProvider "http://localhost:9116"

ckbProvider :: Provider
ckbProvider =
  HttpProvider "http://localhost:9114"

deployedSUDT :: Script
deployedSUDT = Script
  "0x82a4784a46f42916f144bfd1926fda614560e403bc131408881de82fee0724ad"
  "data"
  "0x13146ce73ad549724291df1ecb476c6cc5837a9a1e5393728be71cba9b885027"

deployedSUDTDep :: CellDep
deployedSUDTDep = CellDep
  (OutPoint "0xb8e114fe03ca612c2987f56d6126c87a3aad3647156dbb8b2a16fc9888676776" "0x0")
  Code

getTransactions :: JsonRpc m => SearchKey -> Order -> T.Text -> m SearchResults
getTransactions = remote "get_transactions"

getTransaction :: JsonRpc m => T.Text -> m TxInfo
getTransaction = remote "get_transaction"

getTip :: JsonRpc m => m BlockTip
getTip = remote "get_tip"

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

hexString' :: BS.ByteString -> Maybe HexString
hexString' bs =
  let isValidHex :: Word8 -> Bool
      isValidHex c
        | (48 <= c) && (c < 58)  = True
        | (97 <= c) && (c < 103) = True
        | otherwise              = False
  in if   BS.all isValidHex bs
     then Just (hexString bs)
     else Nothing

-- TODO: Make this a maybe right away
fromHexUtf8 :: Binary a => T.Text -> Maybe a
fromHexUtf8 t = do
  here <- hexString' $ T.encodeUtf8 t
  let bytes =
        toBytes here
  case decodeOrFail $ LBS.fromStrict bytes of
    Left _ -> Nothing
    Right (_, _, a) -> Just a

getMints :: Script -> Tx -> [MintTx]
getMints script (Tx cells outputs) = cs'
  where
    cs = zip cells outputs

    cs' :: [MintTx]
    cs' =
      catMaybes
      $ fmap (\(c, o) -> MintTx (cell_lock c) . BNN.toInteger . unSUDTAmount <$> fromHexUtf8 (rejig o)) cs
