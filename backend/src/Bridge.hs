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
import Servant.Client (ClientM, client, ClientEnv, parseBaseUrl, mkClientEnv, runClientM, Scheme(Http)
                      , BaseUrl(BaseUrl), ClientError)
import Network.HTTP.Client (Manager(..), newManager, managerResponseTimeout, responseTimeoutNone)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Network.Wai.Handler.Warp (run)

import Bridge.Utils

import qualified Bridge.Nervos.Types as CKB
import qualified Bridge.Nervos as CKB

import qualified Bridge.Cardano.Blockfrost as BF
-- TODO Unify these
import qualified Bridge.Cardano.Types as Ada
import qualified Bridge.Cardano as Ada

import Bridge.Nervos.Cli

{-
We need to load the provider
We need to load the contract location
We need to load the listen address
We need to load the multisig address that we will listen on

Multisig Addresss
We will do 3 verifiers with 2 things

collector
ckt1qyq8d80l0z2y40hwkjjhkc68yv48gau2dwlsvzucru

verifiers
ckt1qyqvsv5240xeh85wvnau2eky8pwrhh4jr8ts8vyj37
ckt1qyqywrwdchjyqeysjegpzw38fvandtktdhrs0zaxl4
tx build-multisig-address --sighash-address ckt1qyqvsv5240xeh85wvnau2eky8pwrhh4jr8ts8vyj37 ckt1qyqywrwdchjyqeysjegpzw38fvandtktdhrs0zaxl4  --threshold 1

ckt1qyq8d80l0z2y40hwkjjhkc68yv48gau2dwlsvzucru
ckt1qyqvsv5240xeh85wvnau2eky8pwrhh4jr8ts8vyj37

Here is the multisig info for the current verifiers

lock-arg: 0x901d84b53c1e05fc89a54f5c50346ae267f43aab
lock-hash: 0xe12969536f7c5e0c689af22bd63addafb45da4e1ea6031f317ab34ffc98c53f1
mainnet: ckb1qyqeq8vyk57pup0u3xj57hzsx34wyel5824slztglf
testnet: ckt1qyqeq8vyk57pup0u3xj57hzsx34wyel5824sz84hn4

The deployed contract is code-hash is:
0x82a4784a46f42916f144bfd1926fda614560e403bc131408881de82fee0724ad

The outpoint (where the contract was deployed):
0xb8e114fe03ca612c2987f56d6126c87a3aad3647156dbb8b2a16fc9888676776
0x0

Funds have to be transferred to the multisig address at this time
-}

data VerifierConfig =
  VerifierConfig { verifierConfigNode :: Provider
                 , verifierConfigIndexer :: Provider

                 , verifierNervosMultisigAddress :: CKB.Address
                 , verifierNervosPersonalAddress :: CKB.Address

                 , verifierNervosPassword :: T.Text

                 , verifierNervosDeployedScript :: CKB.DeployedScript
                 , verifierCardanoAddress :: Ada.Address

                 , verifierApiKey :: BF.ApiKey
                 -- TODO(galen): does a verifier need to know its own port?
                 , verifierConfigPort :: Int

                 , verifierMultiSigConfigs :: MultiSigConfigs
                 }
  deriving (Eq)

data CollectorConfig =
  CollectorConfig { collectorConfigNode :: Provider
                  , collectorConfigIndexer :: Provider
                  
                  , collectorNervosMultisigAddress :: CKB.Address
                  , collectorNervosDeployedScript :: CKB.DeployedScript

                  , collectorCardanoAddress :: Ada.Address

                  , collectorApiKey :: BF.ApiKey

                  , collectorMultiSigConfig :: MultiSigConfigs
                  , collectorVerifierUrls :: [String]
                 }
  deriving (Eq)

data Response =
  Response { responseSignature :: Maybe Signature }
  deriving (Eq, Show)

data Request =
  Request { requestLock :: Ada.LockTx }
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

-- TODO(galen): properly set ports 
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
    getFirstSignature (TxFile _ _ (Signatures map)) = headMay . snd =<< (headMay $ M.toList map)
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


-- Hardcoded to use https for better security 
-- TODO(galen): Change this to configure to each URL for verifiers 
myMkClientEnv :: Int -> Manager -> String -> ClientEnv 
myMkClientEnv port manager domain = mkClientEnv manager (BaseUrl Http domain port "")

-- TODO(galen): extend this to take a multisig threshold ?
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

{-
What currently happens

We identify locks without mints

Get all signatures for all mints

build transactions for all mints

sign all mints

send all mints
-}

-- TODO(skylar): This should really come from the verifiers config or something
buildClientEnvs :: Manager -> [ClientEnv]
buildClientEnvs manager =
  [ myMkClientEnv 8119 manager "localhost"
  , myMkClientEnv 8129 manager "localhost"
  -- , myMkClientEnv 8139 manager "localhost"
  ]

-- TODO(skylar): How do we know this is actually in need of minting?
-- actually the verifiers probably cover this case??
runMintFlow :: BridgeM m => CollectorConfig -> Manager -> Ada.LockTx -> m ()
runMintFlow cc manager ltx = do
  responses <- liftIO $ for clientEnvs (runClientM $ requestSignature req)

  let signatures =
        catMaybes $ fmap (join . fmap responseSignature . eitherToMaybe) $ responses

  logDebug $  "Responses: " <> (T.pack . show) responses

  -- TODO(skylar): Big bad hardcode
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
    -- TODO(skylar): Can we share these across all runMintFlow operations
    clientEnvs = buildClientEnvs manager
    multiSigAddress = collectorNervosMultisigAddress cc
    multiSigConfig = collectorMultiSigConfig cc
    MultiSigConfigs multiSigMap = multiSigConfig
    deployedScript = collectorNervosDeployedScript cc
    ckb = collectorConfigNode cc
    indexer = collectorConfigIndexer cc
    verifierUrls = collectorVerifierUrls cc
    apiKey = collectorApiKey cc
    -- verifyTransaction envs req = mapM (runClientM $ requestSignature req) envs

runCollector :: CollectorConfig -> IO ()
runCollector cc = forever $ do
  runBridgeInFile "collector.log" $ do
    locks <- Ada.getLockTxsAt apiKey $ collectorCardanoAddress cc
    logDebug $ "Got lock txns"
    mints <- CKB.getMintTxsAt ckb indexer $ CKB.deployedScriptScript deployedScript
    logDebug $ "Got mints"

    let unMinted = getUnmintedLocks locks mints

    logDebug $ "Found " <> (T.pack . show . length) unMinted <> " unminted lock transactions"

    -- TODO(skylar): Create manager one time and pass in, don't recreate after each invocation
    manager <- liftIO $ newManager $ tlsManagerSettings { managerResponseTimeout = responseTimeoutNone }
    -- TODO(skylar): Paralellize this a whole bunch
    for_ unMinted (runMintFlow cc manager)
  threadDelay $ 120000000
  where
    multiSigAddress = collectorNervosMultisigAddress cc
    multiSigConfig = collectorMultiSigConfig cc
    MultiSigConfigs multiSigMap = multiSigConfig
    deployedScript = collectorNervosDeployedScript cc
    ckb = collectorConfigNode cc
    indexer = collectorConfigIndexer cc
    verifierUrls = collectorVerifierUrls cc
    apiKey = collectorApiKey cc
{-
  logDebug $ "Creating manager"
  manager <- liftIO $ newManager tlsManagerSettings

  let unMinted = getUnmintedLocks locks mints

      requestsToMint :: [Request] 
      requestsToMint = Request <$> unMinted 

      -- TODO(galen): code this to zip with the 5 ports for starting the verifiers
      -- TODO(skylar): This should come from the config
      clientEnvs = [ myMkClientEnv 8009 manager "localhost"
                   , myMkClientEnv 8010 manager "localhost"
                   , myMkClientEnv 8011 manager "localhost"
                   ] 
        
      requestSignatures :: [Request] -> ClientM [Response]
      requestSignatures reqs = mapM requestSignature reqs 

      -- getResponses :: [Request] -> ClientEnv -> IO [Response]
      -- getResponses reqs clientEnv = runClientM (requestSignatures reqs) clientEnv 

      -- requestVerifiersSignatures
      verifyTransaction :: [ClientEnv] -> Request -> IO [Either ClientError Response]
      verifyTransaction envs req = mapM (runClientM $ requestSignature req) envs

  logDebug $ "We have unminted locks: " <> (T.pack . show) unMinted
  -- represents list of lists_A where lists_A is a Maybe Signature; the inner list thererfore
  -- represents whether the transaction should succeed 
  logDebug $ "Fetching signatures from verifers: "

  -- TODO(skylar):
  responses <- liftIO $ mapM (verifyTransaction clientEnvs) requestsToMint

  logDebug $ "Got responses from verifiers: " <> (T.pack . show) responses <> " building txn files"
  
  let
    -- TODO(galen): should we make this a set?
    reqRes :: [(Ada.LockTx, [Either ClientError (Maybe Signature)])]
    reqRes = zip unMinted ((fmap.fmap.fmap) responseSignature responses)

    valid :: [(Ada.LockTx, [Signature])]
    valid = getValidMintTxs reqRes 
  
  mintFilePaths <- mapM (buildMintTxn multiSigAddress verifiersMultiSig multiSigConfig deployedScript) $ fst <$> valid

  -- TODO(galen): make it very clear that the Ada.LockTx -> TxFile =is= ckb_mint 
  let
    toSign :: [(FilePath, [Signature])] 
    toSign = zip mintFilePaths $ snd <$> valid 
    
    addSigsToTxFile :: BridgeM m => [Signature] -> FilePath -> m (Maybe FilePath) 
    addSigsToTxFile signatures path = do
      txn <- liftIO $ decodeFileStrict path
      case txn of
        Just txn' -> do 
          liftIO $ encodeFile path
            $ TxFile txn' multiSigConfig
            $ Signatures
            -- NOTE(skylar): We are assuming that not only there is a lock-arg here, but that it is the first one
            -- we care about, this is likely fine.
            $ M.fromList [((fst . head . M.toList $ multiSigMap), signatures)]
          pure $ Just path
        Nothing -> do
          logError "Failed to read tx file"
          pure Nothing

    signalUsPlease (Just hash) = logDebug $ "Success: " <> hash
    signalUsPlease Nothing = logError "Failed to submit tx file"

  maybeMintFilePaths <- mapM (\(fp, sigs) -> addSigsToTxFile sigs fp) toSign
  for_ (catMaybes maybeMintFilePaths) $ \fp -> do
    logDebug $ "Submitting file: " <> T.pack fp
    result <- submitTxFromFile fp
    signalUsPlease result

  -- liftIO $ threadDelay 1000000
  pure ()
  where
    multiSigAddress = collectorNervosMultisigAddress vc
    multiSigConfig = collectorMultiSigConfig vc
    MultiSigConfigs multiSigMap = multiSigConfig 
    deployedScript = collectorNervosDeployedScript vc
    ckb = collectorConfigNode vc
    indexer = collectorConfigIndexer vc
    verifierUrls = collectorVerifierUrls vc 
    apiKey = collectorApiKey vc
-}
{-
buildMintTx :: BridgeM m => Ada.LockTx -> m ()
buildMintTx (Ada.LockTx _ _ _) = do
  pure ()
-}
getUnmintedLocks :: [Ada.LockTx] -> [CKB.MintTx] -> [Ada.LockTx]
getUnmintedLocks ls ms =
  filter (\lt -> not $ any (comp lt) ms) ls
  where
    comp (Ada.LockTx _ lscr v) (CKB.MintTx mscr v') = lscr == mscr && v == v'
