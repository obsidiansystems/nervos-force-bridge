{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Description: Backend for force-bridge
-}

module Backend where

import Bridge

import Control.Monad
import Control.Monad.IO.Class
import Common.Route
import Common.Bridge

import Backend.Utils

import Obelisk.Backend
import Data.ByteString (ByteString)

import Data.Maybe
import Data.Map (Map, fromList)
import qualified Data.Map as Map

import Control.Lens
import Control.Monad.Log
import Network.Wreq

import Network.Web3
import Network.Web3.Provider
import Network.JsonRpc.TinyClient

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens

import Data.Foldable
import Data.Traversable
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)

import qualified Data.Text as T

import CKB
import CKB.RPC

getAllLockRecords :: ForceM m => m [LockTx]
getAllLockRecords = do
  hashes <- liftIO $ getCollectorTxHashes
  mlocks <- for hashes $ \h -> do
    mScript <- lookupLockPayee h
    paid <- getTxValuePaid h
    let lovelace = Map.findWithDefault 0 Ada paid
    pure $ (\scr -> LockTx h scr lovelace) <$> mScript
  pure $ catMaybes mlocks

-- https://cardano-mainnet.blockfrost.io/api/v0/txs/{hash}/utxos
getTxValuePaid :: ForceM m => TxHash -> m (Map AssetType Integer)
getTxValuePaid (TxHash hash) = do
  let opts = defaults
             & header "project_id" .~ ["testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"]
  r <- liftIO $ asJSON =<< getWith opts url
  pure $ foldl' (Map.unionWith (+)) mempty
       $ fmap (co_amount)
       $ filter ((testContractAddress ==) . co_address)
       $ txo_outputs $ r ^. responseBody
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/txs/" <> T.unpack hash <> "/utxos"

-- TODO better naming?
-- TODO Well the defaults + header is the same everytime we talk to blockfrost
getCollectorTxHashes :: IO [TxHash]
getCollectorTxHashes = do
  let opts = defaults
             & header "project_id" .~ ["testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"]
  r <- asJSON =<< getWith opts url
  pure $ r ^. responseBody
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/addresses/" <> T.unpack testContractAddress <> "/transactions"


-- TODO Fix this name later
lookupLockPayee :: ForceM m => TxHash -> m (Maybe Script)
lookupLockPayee txh@(TxHash hash) = do
  let opts = defaults
             & header "project_id" .~ ["testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"]
  r <- liftIO $ getWith opts url
  let result =
        fmap mintToAddress
        $ preview (responseBody . values . key "json_metadata" . _JSON) r
  case result of
    Nothing -> pure Nothing
    Just ckbAddress -> do
      script <- decodeAddress $ unCKBAddress ckbAddress
      pure $ Just script
  where
    url = "https://cardano-testnet.blockfrost.io/api/v0/txs/" <> T.unpack hash <> "/metadata"

-- TODO How much do we care about signaling failure here?
getAllMintRecords :: IO [MintTx]
getAllMintRecords = do
  result <- runWeb3' ckbIndexerProvider $ getTransactions (SearchKey deployedSUDT Type) Desc "0x64"
  case result of
    Left _ -> do
      pure []
    Right searchresults -> do
      allMints <- runWeb3' ckbProvider $ do
        fmap mconcat <$> for (objects searchresults) $ \thing -> do
          t <- getTransaction . tx_hash $ thing
          pure $ getMints deployedSUDT $ transaction t
      case allMints of
        Left _ -> pure []
        Right a -> pure a

-- Port could extend to domain when we actually launch
startVerifiers :: _ -> IO [Port] 
startVerifiers = do
  let mkConfig port = VerifierConfig
                      nodeProvider
                      indexerProvider
                      _ _ {-<- in comments-}
                      password -- just make one
                      (=<< getsDeployedScript)
                      cardanoAddress
                      apiKey
                      port
  
  startVerifier 8000
  startVerifier 8001
  startVerifier 8002
  startVerifier 8003
  startVerifier 8004
                 
               
  where
    startVerifier port = run port (verifierApplication $ mkConfig port) 

-- | Backend definition for force-bridge
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- flip runLoggingT (putDoc . renderWithSeverity pretty) $ runDevNode "ckb"
      let
        cardanoAddress = undefined
        
        deployedScript = DeployedScript deployedSUDT deployedSUDTDep
        myMultiSigConfig = MultiSigConfig (fromList [(lockArg, verifierAddresses)])
        mkVerifierConfig port thisAddress password = VerifierConfig
                                                     ckbProvider
                                                     ckbIndexerProvider
                                                     (Address "ckt1qyqeq8vyk57pup0u3xj57hzsx34wyel5824sz84hn4")
                                                     thisAddress
                                                     password 
                                                     deployedScript 
                                                     cardanoAddress 
                                                     defaultApiKey 
                                                     port 
                                                     
        myCollectorConfig = CollectorConfig
                            ckbProvider
                            ckbIndexerProvider
                            (Address "ckt1qyqeq8vyk57pup0u3xj57hzsx34wyel5824sz84hn4")
                            deployedScript
                            cardanoAddress
                            defaultApiKey
                            "0x901d84b53c1e05fc89a54f5c50346ae267f43aab"
                            -- ^ might pass in multisig config instead 
                            [] -- we dont need this yet 
                            
      startVerifiers
      runCollector 
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
