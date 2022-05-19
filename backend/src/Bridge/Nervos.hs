{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Bridge.Nervos where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Data.Traversable
import Bridge.Utils

import Bridge.Nervos.Types
-- TODO delete indexer keep RPC
import Bridge.Nervos.RPC
import Network.Web3.Provider (Provider)

import Data.Maybe (catMaybes)
import qualified Basement.Numerical.Number as BNN

import Bridge.Nervos.SUDT

-- TODO How much do we care about signaling failure here?
-- TODO change
getMintTxsAt :: BridgeM m => Provider -> Provider -> Script -> Maybe Script -> m [MintTx]
getMintTxsAt ckb indexer script lockscript = do
  searchData <- return $ case lockscript of
	    Nothing -> getTransactions (SearchKey script Type) Desc "0x64"
            Just lock -> getTransactions' (FilteredSearch (SearchKey lock Lock) [(SearchKey script Type)]) Desc "0x64"
	
  result <- runIndexer indexer searchData
  case result of
    Left err -> do
      logDebug $ "Error: " <> (T.pack . show) err
      pure []
    Right searchresults -> do
      searchObjs <- pure $ searchResults_objects searchresults
      allMints <- runCkb ckb $ do
        fmap mconcat <$> for searchObjs $ \thing -> do
          t <- getTransaction . txRecord_tx_hash $ thing
          pure $ getMints script (txInfo_transaction t)  
      case allMints of
        Left err -> do
          logDebug $ "Error: " <> (T.pack . show) err
          pure []
        Right a -> do
          pure $ reverse a <*> (txRecord_tx_hash <$> searchObjs)

-- | Helper function to pull mint information related to a script from a transaction
getMints :: Script -> Tx -> [T.Text -> MintTx]
getMints script (Tx cells outputs) = cs'
  where
    cs = zip cells outputs

    cs' :: [T.Text -> MintTx]
    cs' =
      catMaybes
      $ fmap (\(c, o) -> MintTx (cell_lock c) . BNN.toInteger . unSUDTAmount <$> fromHexUtf8 (rejig o)) cs
