{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | 

module Bridge where

import Control.Lens
import Control.Monad
import Control.Monad.Log
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Network.Web3.Provider (Provider)
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Map as M

import Data.Foldable
import Data.Traversable

import Data.Aeson
import Data.Aeson.TH

import Servant
import Servant.Client (ClientM, client, ClientEnv, mkClientEnv, runClientM, Scheme(Http)
                      , BaseUrl(BaseUrl), ClientError)
import Network.HTTP.Client (Manager, newManager, managerResponseTimeout, responseTimeoutNone)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Network.Wai.Handler.Warp (run)

import Bridge.Utils

import qualified Bridge.Nervos.Types as CKB
import qualified Bridge.Nervos as CKB

import qualified Bridge.Cardano.Blockfrost as BF
import qualified Bridge.Cardano.Types as Ada
import qualified Bridge.Cardano as Ada

import Bridge.Nervos.Cli

data VerifierConfig =
  VerifierConfig { verifierConfigNode :: !Provider
                 , verifierConfigIndexer :: !Provider

                 , verifierNervosMultisigAddress :: !CKB.Address
                 , verifierNervosPersonalAddress :: !CKB.Address

                 , verifierNervosPassword :: !T.Text

                 , verifierNervosDeployedScript :: !CKB.DeployedScript
                 , verifierCardanoAddress :: !Ada.Address

                 , verifierApiKey :: !BF.ApiKey

                 , verifierConfigPort :: !Int

                 , verifierMultiSigConfigs :: !MultiSigConfigs
                 }
  deriving (Eq)

data CollectorConfig =
  CollectorConfig { collectorConfigNode :: !Provider
                  , collectorConfigIndexer :: !Provider
                  
                  , collectorNervosMultisigAddress :: !CKB.Address
                  , collectorNervosDeployedScript :: !CKB.DeployedScript

                  , collectorCardanoAddress :: !Ada.Address

                  , collectorApiKey :: !BF.ApiKey

                  , collectorMultiSigConfig :: !MultiSigConfigs
                  , collectorVerifierUrls :: ![String]
                 }
  deriving (Eq)

data Response =
  Response { responseSignature :: !(Maybe Signature) }
  deriving (Eq, Show)

data Request =
  Request { requestLock :: !Ada.LockTx }
  deriving (Eq, Show)

deriveJSON defaultOptions ''Request
deriveJSON defaultOptions ''Response

type VerifierAPI =
 "sign" :> ReqBody '[JSON] Request :> Get '[JSON] Response

verifierApiProxy :: Proxy VerifierAPI
verifierApiProxy = Proxy

verifierServer :: VerifierConfig -> Server VerifierAPI
verifierServer = handleSignatureRequest

verifierApplication :: VerifierConfig -> Application
verifierApplication vc = serve verifierApiProxy $ verifierServer vc

runVerifierServer :: MonadIO m => VerifierConfig -> m ()
runVerifierServer vc = liftIO $ run (verifierConfigPort vc) (verifierApplication vc)

requestSignature :: Request -> ClientM (Response)
requestSignature = client verifierApiProxy

handleSignatureRequest :: VerifierConfig -> Request -> Servant.Handler Response
handleSignatureRequest vc (Request lockTx) = liftIO $ do
  runBridgeInFile ("verifier" <>  show (verifierConfigPort vc) <> ".log") $ do
    locks <- Ada.getLockTxsAt apiKey $ verifierCardanoAddress vc
    mints <- CKB.getMintTxsAt ckb indexer $ CKB.deployedScriptScript deployedScript
    let
      foundLock = isJust $ headMay $ filter (== lockTx) $ getUnmintedLocks locks mints

    logDebug $ "Looking for locktx : " <> (T.pack . show) lockTx
    logDebug $ "Looked up locktx found: " <> (T.pack . show) foundLock
    Response <$> case foundLock of
      False -> pure Nothing
      True -> do
        logDebug "Building and signing"
        txFile <- buildMintTxn multiSigAddress verifiersMultiSig multiSigConfigs deployedScript lockTx
        logDebug $ "Created tx file: " <> T.pack txFile
        signTxFile txFile sigAddress pass
        logDebug $ "Decoding file to get signature: " <> T.pack txFile
        txn <- liftIO $ decodeFileStrict txFile
        case getFirstSignature =<< txn of 
          Just sig -> do
            logDebug "Successfully got signature"
            pure . Just $ sig
          Nothing -> do
            logDebug $ "Failed to get signature from file: " <> T.pack txFile
            pure Nothing
  where
    getFirstSignature (TxFile _ _ (Signatures map')) = headMay . snd =<< (headMay $ M.toList map')
    deployedScript = verifierNervosDeployedScript vc
    multiSigAddress = verifierNervosMultisigAddress vc
    sigAddress = verifierNervosPersonalAddress vc
    multiSigConfigs = verifierMultiSigConfigs vc
    pass = verifierNervosPassword vc

    ckb = verifierConfigNode vc
    indexer = verifierConfigIndexer vc

    apiKey = verifierApiKey vc

headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay _ = Nothing


myMkClientEnv :: Int -> Manager -> String -> ClientEnv
myMkClientEnv port manager domain = mkClientEnv manager (BaseUrl Http domain port "")

getValidMintTxs :: [(Ada.LockTx, [Either ClientError (Maybe Signature)])] -> [(Ada.LockTx, [Signature])] 
getValidMintTxs txnsResponses = filter signaturesAtLeast2
                                $ fmap (\(tx, emSigs) -> (tx, catMaybes $ fmap (join . eitherToMaybe) emSigs)) 
                                $ txnsResponses
  where
    signaturesAtLeast2 :: (Ada.LockTx, [Signature]) -> Bool
    signaturesAtLeast2 (_, sigs) = length sigs > 1

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing

buildClientEnvs :: Manager -> [ClientEnv]
buildClientEnvs manager =
  [ myMkClientEnv 8119 manager "localhost"
  , myMkClientEnv 8129 manager "localhost"
  , myMkClientEnv 8139 manager "localhost"
  ]

runMintFlow :: BridgeM m => CollectorConfig -> Manager -> Ada.LockTx -> m ()
runMintFlow cc manager ltx = do
  responses <- liftIO $ for clientEnvs (runClientM $ requestSignature req)

  let signatures =
        catMaybes $ fmap (join . fmap responseSignature . eitherToMaybe) $ responses

  logDebug $  "Responses: " <> (T.pack . show) responses

  case length signatures >= 2 of
    False -> do
        logDebug $ "Not enough signatures present for mint, abandoning: " <> (T.pack . show) signatures

    True -> do
      fp <- buildMintTxn multiSigAddress verifiersMultiSig multiSigConfig deployedScript ltx
      mTxFile <- liftIO $ decodeFileStrict fp
      case mTxFile of
        Just txFile -> do
          liftIO $ encodeFile fp $ txFile
            & txFile_signatures .~
              Signatures (M.fromList [((fst . head . M.toList $ multiSigMap), take 2 signatures)])
          pure ()
        Nothing -> do
          logError "Failed to read tx file"
          pure ()
      logDebug $ "Submitting file: " <> T.pack fp
      result <- submitTxFromFile fp
      signalUsPlease result
      pure ()
  where
    signalUsPlease (Just hash) = logDebug $ "Success: " <> hash
    signalUsPlease Nothing = logError "Failed to submit tx file"

    req = Request ltx
    clientEnvs = buildClientEnvs manager
    multiSigAddress = collectorNervosMultisigAddress cc
    multiSigConfig = collectorMultiSigConfig cc
    MultiSigConfigs multiSigMap = multiSigConfig
    deployedScript = collectorNervosDeployedScript cc

runCollector :: CollectorConfig -> IO ()
runCollector cc = forever $ do
  runBridgeInFile "collector.log" $ do
    locks <- Ada.getLockTxsAt apiKey $ collectorCardanoAddress cc
    logDebug $ "Got lock txns"
    mints <- CKB.getMintTxsAt ckb indexer $ CKB.deployedScriptScript deployedScript
    logDebug $ "Got mints"

    let unMinted = getUnmintedLocks locks mints

    logDebug $ "Found " <> (T.pack . show . length) unMinted <> " unminted lock transactions"

    manager <- liftIO $ newManager $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
    for_ unMinted (runMintFlow cc manager)
  threadDelay $ 120000000
  where
    deployedScript = collectorNervosDeployedScript cc
    ckb = collectorConfigNode cc
    indexer = collectorConfigIndexer cc
    apiKey = collectorApiKey cc

getUnmintedLocks :: [Ada.LockTx] -> [CKB.MintTx] -> [Ada.LockTx]
getUnmintedLocks ls ms =
  reverse $ filter (\lt -> not $ any (comp lt) ms) ls
  where
    comp (Ada.LockTx _ lscr v) (CKB.MintTx mscr v') = lscr == mscr && v == v'
