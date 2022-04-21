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
import Bridge.Utils
import Bridge.Cardano.Blockfrost (defaultApiKey)
import qualified Bridge.Cardano.Types as Ada
import qualified Bridge.Nervos.Types as CKB
import Bridge.Nervos.Cli (MultiSigConfigs(..), MultiSigConfig(..), ckbCliPath)

import Obelisk.Backend

import Data.Map (fromList)

import Network.Wai.Handler.Warp (run, Port)
import Control.Concurrent (forkIO)
import System.Process

import Data.Foldable

import qualified Data.Text as T

import Bridge.Nervos.RPC (ckbIndexerProvider, ckbProvider)

ensureObsidianNode :: BridgeM m => m ()
ensureObsidianNode = do
  logDebug "Setting Obsidian node"
  _ <- liftIO $ readProcess ckbCliPath [] "config --url http://obsidian.webhop.org:9114\nexit\n"
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
                            []
        
        startVerifier :: (CKB.Address, (Port, T.Text)) -> IO () 
        startVerifier conf@(_, (port, _)) = void $ forkIO $ run port $ verifierApplication $ mkVerifierConfig conf

      _ <- liftIO $ do
        runBridge $ do
          ensureObsidianNode
          registerVerifiers

        mapM_ startVerifier verifierCredentials
        forkIO $ runCollector myCollectorConfig

      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
