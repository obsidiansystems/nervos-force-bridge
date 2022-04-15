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

import Common.Nervos
import Bridge.Nervos.Types
import Bridge.Utils

-- data SearchResults = SearchResults
--   { searchResults_last_cursor :: T.Text
--   , searchResults_objects :: [TxRecord]
--   }
--   deriving (Eq, Show)

-- data TxRecord = TxRecord
--   { txRecord_tx_hash :: T.Text
--   }
--   deriving (Eq, Show)


--deriveJSON (scrubPrefix "txRecord_") ''TxRecord
--deriveJSON (scrubPrefix "searchResults_") ''SearchResults
 
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
getTransaction :: JsonRpc m => T.Text -> m CkbTxInfo
getTransaction = remote "get_transaction"

getLiveCells :: JsonRpc m => SearchKey -> Order -> T.Text -> m GetCellsResult
getLiveCells = remote "get_cells"

getLiveCells' :: JsonRpc m => SearchKey -> Order -> T.Text -> m Value
getLiveCells' = remote "get_cells"
