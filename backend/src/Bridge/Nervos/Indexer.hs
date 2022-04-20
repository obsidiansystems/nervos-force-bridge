{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Bridge.Nervos.Indexer where

import Control.Monad.IO.Class

import Network.Web3.Provider
import Network.JsonRpc.TinyClient

import Data.Aeson
import Data.Aeson.TH

import qualified Data.Text as T

import Bridge.Nervos.Types
import Bridge.Utils

data SearchResults = SearchResults
  { searchResults_last_cursor :: !T.Text
  , searchResults_objects :: ![TxRecord]
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
  { searchKey_script :: !Script
  , searchKey_script_type :: !ScriptType
  }

data TxRecord = TxRecord
  { txRecord_tx_hash :: !T.Text
  }
  deriving (Eq, Show)

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


deriveJSON (scrubPrefix "txRecord_") ''TxRecord
deriveJSON (scrubPrefix "searchKey_") ''SearchKey
deriveJSON (scrubPrefix "searchResults_") ''SearchResults

runIndexer :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
runIndexer p =
  liftIO . runWeb3' p

getTransactions :: JsonRpc m => SearchKey -> Order -> T.Text -> m SearchResults
getTransactions = remote "get_transactions"
