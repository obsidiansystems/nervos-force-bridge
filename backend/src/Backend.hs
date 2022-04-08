{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Description: Backend for force-bridge
-}

module Backend where

import Bridge

import System.IO (hFlush)
import System.IO.Temp (withTempFile)
import Data.Text.IO (hPutStr)
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (SomeException, try)
import Common.Route
import Common.Bridge
import Bridge.Utils
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
import Prettyprinter.Render.Text (putDoc, hPutDoc)

import qualified Data.Text as T

import CKB
import CKB.RPC hiding (deployedSUDT, deployedSUDTDep)

{-
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
-}
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

-- CKB_CLI_HOME
-- TODO rework this to be in the interactive cli
ensureObsidianNode :: BridgeM m => m ()
ensureObsidianNode = do
  logDebug "Setting Obsidian node"
  liftIO $ readProcess ckbCliPath [] "config --url http://obsidian.webhop.org:9114\nexit\n"
  logDebug "Obsidian node set"
  pure ()

registerVerifierAddress :: BridgeM m => T.Text -> T.Text -> m ()
registerVerifierAddress pk pass = do
  liftIO $ withTempFile "." "pk" $ \fp h -> do
    hPutStr h pk
    hFlush h
    let
      opts = [ "account"
             , "import"
             , "--privkey-path"
             , fp
             ]
    _ :: Either SomeException String <- try $ readProcess ckbCliPath opts $ T.unpack pass
    pure ()

verifierPrivateKeys :: [T.Text]
verifierPrivateKeys =
  [ "fcfee1173b5cf89b813ad92fda5eb6230bb682c1053461046f5ce23bcde08cea"
  , "7b983ba820ff12f4a0b214b4574117a34bc921ac87c4c90c73103b15430392f7"
  , "f4922678338ea5df36da50203d4d1df27ecf779c5625d607848e968e629a1e15"
  ]

registerVerifiers :: BridgeM m => m ()
registerVerifiers = do
  for_ verifierPrivateKeys (flip registerVerifierAddress "pass")

-- | Backend definition for force-bridge
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- flip runLoggingT (putDoc . renderWithSeverity pretty) $ runDevNode "ckb"
      let
        cardanoAddress = Ada.Address "addr_test1qqvyvv446w768jj42wxrh99mpmk5kd2qppst0yma8qesllldkdcxe8fngwj6m2f9uk5k8unf94tzzryz7kujnnew29xse6rxsu"

        mSigLockArg = "0xc099a986b4c66a590b09011e3b139bf5a73e2e50"
        verifierCredentials = [ (CKB.Address "ckt1qyqw2mw2tx493vhtcf5g7rzxggldfxtvn2ksdheprt", (8119, "pass"))
                              , (CKB.Address "ckt1qyqxak478atwzfx0kqqa4sepnfqfgd7x2kesjy0v6k", (8129, "pass"))
                              , (CKB.Address "ckt1qyq0222yxth2mtj3jmyt9uzkfxkrf4yehtjs5xvgnk", (8139, "pass"))
                              ]
        verifierAddresses = fst <$> verifierCredentials 
        deployedScript = CKB.DeployedScript CKB.deployedSUDT CKB.deployedSUDTDep
        myMultiSigConfigs = MultiSigConfigs (fromList [(mSigLockArg
                                                       , MultiSigConfig verifierAddresses 0 2)])
        mkVerifierConfig (thisAddress, (port, password)) = VerifierConfig
          ckbProvider
          ckbIndexerProvider
          (CKB.Address "ckt1qyqupxdfs66vv6jepvysz83mzwdltfe79egqa4geg5")
          thisAddress
          password 
          deployedScript 
          cardanoAddress 
          defaultApiKey 
          port
          myMultiSigConfigs
                                                     
        myCollectorConfig = CollectorConfig
                            ckbProvider
                            ckbIndexerProvider
                            (CKB.Address "ckt1qyqupxdfs66vv6jepvysz83mzwdltfe79egqa4geg5")
                            deployedScript
                            cardanoAddress
                            defaultApiKey
                            myMultiSigConfigs 
                            -- ^ might pass in multisig config instead 
                            [] -- we dont need this yet 
        
        startVerifier :: (CKB.Address, (Port, T.Text)) -> IO () 
        startVerifier conf@(a, (port, p)) = void $ forkIO $ run port $ verifierApplication $ mkVerifierConfig conf

      liftIO $ do
        runBridge $ do
          ensureObsidianNode
          registerVerifiers

        mapM_ startVerifier verifierCredentials
        forkIO $ runCollector myCollectorConfig


      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
