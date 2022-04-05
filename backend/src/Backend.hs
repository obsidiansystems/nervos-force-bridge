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
import Bridge.Cardano.Blockfrost (defaultApiKey)
import qualified Bridge.Cardano.Types as Ada
import qualified Bridge.Nervos.Types as CKB -- (Address(..), DeployedScript(..), deployedSUDT, deployedSUDTDep)
import Bridge.Nervos.Cli (MultiSigConfigs(..), MultiSigConfig(..), ckbCliPath)


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
import Network.Wai.Handler.Warp (run, Port)
import Control.Concurrent (forkIO)
import System.Process

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Lens

import Data.Foldable
import Data.Traversable
import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc)

import qualified Data.Text as T

import CKB
import CKB.RPC hiding (deployedSUDT, deployedSUDTDep)

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

-- -- TODO How much do we care about signaling failure here?
-- getAllMintRecords :: IO [MintTx]
-- getAllMintRecords = do
--   result <- runWeb3' ckbIndexerProvider $ getTransactions (SearchKey deployedSUDT Type) Desc "0x64"
--   case result of
--     Left _ -> do
--       pure []
--     Right searchresults -> do
--       allMints <- runWeb3' ckbProvider $ do
--         fmap mconcat <$> for (objects searchresults) $ \thing -> do
--           t <- getTransaction . tx_hash $ thing
--           pure $ getMints deployedSUDT $ transaction t
--       case allMints of
--         Left _ -> pure []
--         Right a -> pure a

-- Port could extend to domain when we actually launch





setCkbCliConfig :: IO ()
setCkbCliConfig = do
  let
    opts = [ "--url"
           , "http://obsidian.webhop.org:9114"
           , "--no-sync" -- otherwise our application will hang for ever 
           ]

  readProcess ckbCliPath opts ""
  pure ()

-- | Backend definition for force-bridge
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- flip runLoggingT (putDoc . renderWithSeverity pretty) $ runDevNode "ckb"
      let
        cardanoAddress = Ada.Address "addr_test1qqvyvv446w768jj42wxrh99mpmk5kd2qppst0yma8qesllldkdcxe8fngwj6m2f9uk5k8unf94tzzryz7kujnnew29xse6rxsu"

        mSigLockArg = "0xc099a986b4c66a590b09011e3b139bf5a73e2e50"
        verifierCredentials = [ (CKB.Address "ckt1qyqw2mw2tx493vhtcf5g7rzxggldfxtvn2ksdheprt", (8003, "pass"))
                              , (CKB.Address "ckt1qyqxak478atwzfx0kqqa4sepnfqfgd7x2kesjy0v6k", (8001, "pass"))
                              , (CKB.Address "ckt1qyq0222yxth2mtj3jmyt9uzkfxkrf4yehtjs5xvgnk", (8002, "pass"))
                              ]
        verifierAddresses = fst <$> verifierCredentials 
        deployedScript = CKB.DeployedScript CKB.deployedSUDT CKB.deployedSUDTDep
        myMultiSigConfigs = MultiSigConfigs (fromList [(mSigLockArg
                                                       , MultiSigConfig verifierAddresses 0 2)])
        mkVerifierConfig (thisAddress, (port, password)) = VerifierConfig
          ckbProvider
          ckbIndexerProvider
          (CKB.Address "ckt1qyqeq8vyk57pup0u3xj57hzsx34wyel5824sz84hn4")
          thisAddress
          password 
          deployedScript 
          cardanoAddress 
          defaultApiKey 
          port 
                                                     
        myCollectorConfig = CollectorConfig
                            ckbProvider
                            ckbIndexerProvider
                            (CKB.Address "ckt1qyqeq8vyk57pup0u3xj57hzsx34wyel5824sz84hn4")
                            deployedScript
                            cardanoAddress
                            defaultApiKey
                            myMultiSigConfigs 
                            -- ^ might pass in multisig config instead 
                            [] -- we dont need this yet 
        
        startVerifier :: (CKB.Address, (Port, T.Text)) -> IO () 
        startVerifier conf@(a, (port, p)) = void $ forkIO $ run port $ verifierApplication $ mkVerifierConfig conf
  
      liftIO $ setCkbCliConfig 
      liftIO $ mapM startVerifier verifierCredentials
      liftIO $ print "hey"
      serve $ const $ return ()
      flip runLoggingT (putDoc . renderWithSeverity pretty) $ runCollector myCollectorConfig
      liftIO $ print "hello"
      
  , _backend_routeEncoder = fullRouteEncoder
  }
