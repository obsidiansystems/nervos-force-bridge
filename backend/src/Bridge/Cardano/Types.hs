{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 

module Bridge.Cardano.Types where

import GHC.Generics
import Data.Traversable
import Text.Read (readMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.TH

import Bridge.Utils
import qualified Bridge.Nervos.Types as CKB

-- TODO(skylar): This is a testnet address
newtype Address =
  Address { unAddress :: T.Text }
  deriving (Eq, Show)

newtype TxHash =
  TxHash { unTxHash :: T.Text }
  deriving (Eq, Show, Generic)

-- TODO(skylar): Steal this from cardano
data AssetType
  = Ada
  | AssetName T.Text
  deriving (Eq, Show, Read, Ord, Generic)

data TxOutput = TxOutput
  { txOutput_address :: T.Text
  , txOutput_amount :: Map AssetType Integer
  }
  deriving (Eq, Show, Generic)

data LockTx =
  LockTx { lockTxHash :: TxHash
         , lockTxLockScript :: CKB.Script
         , lockTxLovelace :: Integer
         }
  deriving (Eq, Show)

data LockMetadata = LockMetadata
  { mintToAddress :: T.Text
  }
  deriving (Eq, Show)

instance FromJSON TxHash where
  parseJSON = withObject "TxHash" $ \o ->
    TxHash <$> o .: "tx_hash"

instance ToJSON TxHash

deriveJSON defaultOptions ''LockTx
deriveJSON defaultOptions ''AssetType
deriveJSON defaultOptions ''LockMetadata
instance ToJSONKey AssetType

instance FromJSON TxOutput where
  parseJSON = withObject "TxOutput" $ \v -> do
    address <- v .: "address"
    lv <- v .: "amount"
    values <- fmap mconcat <$> for lv $ \e -> do
      assetType <- assetTypeFromText <$> (e .: "unit")
      mQuantity <- readMaybe <$> e .: "quantity"
      case mQuantity of
        Nothing -> fail "Not a valid integer"
        Just quantity -> do
          pure $ Map.singleton assetType quantity
    pure $ TxOutput address values

instance ToJSON TxOutput

assetTypeFromText :: T.Text -> AssetType
assetTypeFromText "lovelace" = Ada
assetTypeFromText n = AssetName n
