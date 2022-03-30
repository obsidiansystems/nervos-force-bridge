{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | 

module Bridge where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Network.Web3.Provider (Provider)
import qualified Data.Text as T
import Data.Maybe

import Data.Aeson
import Data.Aeson.TH

import Servant
import Servant.Client (ClientM, client)
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
                 , verifierConfigPort :: Int
                 }
  deriving (Eq)

data CollectorConfig =
  CollectorConfig { collectorConfigNode :: Provider
                  , collectorConfigIndexer :: Provider

                  , collectorNervosDeployedScript :: CKB.DeployedScript

                  , collectorCardanoAddress :: Ada.Address

                  , collectorApiKey :: BF.ApiKey

                  , collectorVerifierUrls :: [String]
                 }
  deriving (Eq)

newtype Signature =
  Signature { unSignature :: T.Text }
  deriving (Eq, Show)

data Response =
  Response { responseSignature :: Maybe Signature }
  deriving (Eq, Show)

data Request =
  Request { requestLock :: Ada.LockTx }
  deriving (Eq, Show)

deriveJSON defaultOptions ''Signature
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
handleSignatureRequest vc (Request lockTx) =
  liftIO $ runBridge $ do
    locks <- Ada.getLockTxsAt apiKey $ verifierCardanoAddress vc
    mints <- CKB.getMintTxsAt ckb indexer $ CKB.deployedScriptScript deployedScript
    let
      foundLock = isJust $ headMay $ filter (== lockTx) $ getUnmintedLocks locks mints

    Response <$> case foundLock of
      False -> pure Nothing
      True -> do
        txFile <- buildMintTxn multiSigAddress deployedScript lockTx
        addSignature txFile sigAddress pass
        -- TODO(skylar): Extend the Signature/MultiSig Configs and addSignature functions to get back the signature
        -- return it here
        pure Nothing

  where
    deployedScript = verifierNervosDeployedScript vc
    multiSigAddress = verifierNervosMultisigAddress vc
    sigAddress = verifierNervosPersonalAddress vc
    pass = verifierNervosPassword vc

    ckb = verifierConfigNode vc
    indexer = verifierConfigIndexer vc

    apiKey = verifierApiKey vc

headMay :: [a] -> Maybe a
headMay (x:_) = Just x
headMay _ = Nothing

runCollector :: BridgeM m => CollectorConfig -> m ()
runCollector vc = forever $ do
  locks <- Ada.getLockTxsAt apiKey $ collectorCardanoAddress vc
  mints <- CKB.getMintTxsAt ckb indexer $ CKB.deployedScriptScript deployedScript

  let unMinted =
        getUnmintedLocks locks mints

  -- TODO(skylar): For each verifier call the requestSignature client function above, providing the endpoint
  -- this will give you the list of signatures you need

  liftIO $ threadDelay 1000000
  pure ()
  where
    deployedScript = collectorNervosDeployedScript vc
    ckb = collectorConfigNode vc
    indexer = collectorConfigIndexer vc

    apiKey = collectorApiKey vc

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
