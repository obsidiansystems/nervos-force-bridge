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

import Common.Bridge (AdaTxHash(..))
import Bridge.Utils
import Bridge.Cardano.Types
-- TODO export this from bridge utils
-- import Control.Monad.Log

-- type Value = Map AssetType Integer

-- TODO differentiate between testnet and mainnet
newtype ApiKey =
  ApiKey { unApiKey :: BS.ByteString }
  deriving (Eq)

-- TODO Only used for this one thing
data TxOutputs = TxOutputs
  { txo_hash :: T.Text
  , txo_outputs :: [TxOutput]
  }
  deriving (Eq, Show)

deriveJSON (scrubPrefix "txo_") ''TxOutputs

defaultApiKey :: ApiKey
defaultApiKey = ApiKey "testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"
-- "testnetHf22J7FWl70gaYZljRwEFS7oE6mKuULF"  --  "testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"

-- TODO failure handling for blockfrost
getTransactions :: BridgeM m => ApiKey -> Address -> m [AdaTxHash]
getTransactions (ApiKey k) (Address addr) = do
  -- logDebug $ "Fetching transactions for address: " <> addr
  let opts = defaults
             & header "project_id" .~ [k]
  r <- liftIO $ asJSON =<< getWith opts url
  -- logDebug $ "Excuse me"
  pure $ r ^. responseBody
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/addresses/" <> T.unpack addr <> "/transactions"

getTransactionMetadata :: (BridgeM m, FromJSON a, ToJSON a) => ApiKey -> AdaTxHash -> m (Maybe a)
getTransactionMetadata (ApiKey k) (AdaTxHash hash) = do
  -- logDebug $ "Fetching metadata for tx: " <> hash
  let opts = defaults
             & header "project_id" .~ [k]
  r <- liftIO $ getWith opts url
  pure $ preview (responseBody . values . key "json_metadata" . _JSON) r
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/txs/" <> T.unpack hash <> "/metadata"

-- TODO split into get UTXOS and something else...
getValuePaidTo :: (BridgeM m) => ApiKey -> Address -> AdaTxHash -> m (Map AssetType Integer)
getValuePaidTo (ApiKey k) (Address addr) (AdaTxHash hash) = do
  -- logDebug $ "Fetching value paid to " <> addr <> " in tx: " <> hash
  let opts = defaults
             & header "project_id" .~ [k]
  r <- liftIO $ asJSON =<< getWith opts url
  pure $ foldl' (Map.unionWith (+)) mempty
       $ fmap (txOutput_amount)
       $ filter ((addr ==) . txOutput_address)
       $ txo_outputs $ r ^. responseBody
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/txs/" <> T.unpack hash <> "/utxos"
