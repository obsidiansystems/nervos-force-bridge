{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- |

module Bridge.Nervos.RPC where

import Control.Monad.IO.Class

import Network.Web3.Provider
import Network.JsonRpc.TinyClient

import Data.Aeson
import Data.Aeson.TH

import qualified Data.Text as T

import Bridge.Nervos.Types
import Bridge.Utils

data Cell = Cell
  { cell_capacity :: T.Text
  , cell_lock :: Script
  , cell_type :: Maybe Script
  }
  deriving (Eq, Show)

data TxInfo = TxInfo
  { txInfo_transaction :: Tx
  }
  deriving (Eq, Show)

data Tx = Tx
  { tx_outputs :: [Cell]
  , tx_outputs_data :: [T.Text]
  }
  deriving (Eq, Show)

data SearchResults = SearchResults
  { searchResults_last_cursor :: T.Text
  , searchResults_objects :: [TxRecord]
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
  { searchKey_script :: Script
  , searchKey_script_type :: ScriptType
  -- TODO Do we want a filter?
  }

data TxRecord = TxRecord
  { txRecord_tx_hash :: T.Text
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
deriveJSON (scrubPrefix "tx_") ''Tx
deriveJSON (scrubPrefix "txInfo_") ''TxInfo
deriveJSON (scrubPrefix "cell_") ''Cell

{-deriveJSON (scrubPrefix "txRecord_") ''TxRecord
deriveJSON (scrubPrefix "searchKey_") ''SearchKey

deriveJSON (scrubPrefix "searchResults_") ''Searc
-}

-- TODO these aren't actually different...
runIndexer :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
runIndexer p =
  liftIO . runWeb3' p

runCkb :: MonadIO m => Provider -> Web3 a -> m (Either Web3Error a)
runCkb p =
  liftIO . runWeb3' p

getTransactions :: JsonRpc m => SearchKey -> Order -> T.Text -> m SearchResults
getTransactions = remote "get_transactions"

-- TODO How to make this work with a custom type? Custom json instance??
getTransaction :: JsonRpc m => T.Text -> m TxInfo
getTransaction = remote "get_transaction"

getLiveCells :: JsonRpc m => SearchKey -> Order -> T.Text -> m GetCellsResult
getLiveCells = remote "get_cells"

getLiveCells' :: JsonRpc m => SearchKey -> Order -> T.Text -> m Value
getLiveCells' = remote "get_cells"
