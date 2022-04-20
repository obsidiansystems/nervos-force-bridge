{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 

module Bridge.Cardano.Blockfrost where

import qualified Data.Text as T

import Data.List
import Network.Wreq
import Control.Lens
import qualified Data.ByteString as BS

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens

import Data.Map (Map)
import qualified Data.Map as Map

import Bridge.Utils
import Bridge.Cardano.Types

newtype ApiKey =
  ApiKey { unApiKey :: BS.ByteString }
  deriving (Eq)

data TxOutputs = TxOutputs
  { txo_hash :: !T.Text
  , txo_outputs :: ![TxOutput]
  }
  deriving (Eq, Show)

deriveJSON (scrubPrefix "txo_") ''TxOutputs

defaultApiKey :: ApiKey
defaultApiKey = ApiKey -- "testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"
  "testnetHf22J7FWl70gaYZljRwEFS7oE6mKuULF"

getTransactions :: BridgeM m => ApiKey -> Address -> m [TxHash]
getTransactions (ApiKey k) (Address addr) = do
  let opts = defaults
             & header "project_id" .~ [k]
  r <- liftIO $ asJSON =<< getWith opts url
  pure $ r ^. responseBody
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/addresses/" <> T.unpack addr <> "/transactions"

getTransactionMetadata :: (BridgeM m, FromJSON a, ToJSON a) => ApiKey -> TxHash -> m (Maybe a)
getTransactionMetadata (ApiKey k) (TxHash hash) = do
  let opts = defaults
             & header "project_id" .~ [k]
  r <- liftIO $ getWith opts url
  pure $ preview (responseBody . values . key "json_metadata" . _JSON) r
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/txs/" <> T.unpack hash <> "/metadata"

getValuePaidTo :: (BridgeM m) => ApiKey -> Address -> TxHash -> m (Map AssetType Integer)
getValuePaidTo (ApiKey k) (Address addr) (TxHash hash) = do
  let opts = defaults
             & header "project_id" .~ [k]
  r <- liftIO $ asJSON =<< getWith opts url
  pure $ foldl' (Map.unionWith (+)) mempty
       $ fmap (txOutput_amount)
       $ filter ((addr ==) . txOutput_address)
       $ txo_outputs $ r ^. responseBody
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/txs/" <> T.unpack hash <> "/utxos"
