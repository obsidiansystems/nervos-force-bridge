{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CKB (runDevNode) where

import Control.Monad
import Control.Monad.IO.Class

import GHC.Generics

import System.Which
import System.Process
import System.Directory

import Data.Bool
import Data.List (intercalate)

import GHC.IO.Handle
import Obelisk.Configs
import qualified Obelisk.ExecutableConfig.Lookup as Lookup

import System.Directory
import System.IO.Temp
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import Data.Aeson.TH

import Control.Concurrent
import Control.Monad.Log

import Toml.Codec ( TomlCodec
                  , HasCodec
                  , genericCodec
                  )
import qualified Toml.Codec as Toml

import CKB.Types
import CKB.Config
import CKB.Capsule

import Data.Attoparsec.Text as A

import Backend.Utils

-- TODO(skylar): DevNode type, what goes in here?
data DevNode = DevNode
  { devNodeMiner :: Account
  , devNodeGenesis1 :: Account
  , devNodeGenesis2 :: Account
  }
  deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''DevNode

procCli :: FilePath -> [String] -> CreateProcess
procCli workingDirectory args =
  addEnvironmentVariable ("CKB_CLI_HOME", workingDirectory)
  $ proc ckbCliPath args

addEnvironmentVariable :: (String, String) -> CreateProcess -> CreateProcess
addEnvironmentVariable = addEnvironmentVariables . pure

addEnvironmentVariables :: [(String, String)] -> CreateProcess -> CreateProcess
addEnvironmentVariables args cp =
  cp { env = env cp <> Just args }

relativeCkbHome :: FilePath -> FilePath
relativeCkbHome = (<> "/.ckb-cli")

-- TODO(skylar): How do we handle the working directory
createNewAccount :: MonadIO m => FilePath -> m Account
createNewAccount path = liftIO $ do
  putStrLn $ show $ relativeCkbHome path
  d <- readCreateProcess (procCli (relativeCkbHome path) ["account", "new"]) input
  let Right a = parseOnly account . T.pack $ d
  pure a
  where
    pass = "hello"
    input = intercalate "\n" [pass, pass]

accountFromPrivateKey :: ForceM m => FilePath -> T.Text -> m Account
accountFromPrivateKey path pk = do
  liftIO $ withTempFile "." "pk" $ \fp handle -> do
    T.hPutStr handle pk
    hFlush handle
    d <- readCreateProcess (procCli (relativeCkbHome path) ["account"
                                                           , "import"
                                                           , "--privkey-path"
                                                           , fp
                                                           ]) "hello\nhello"
    let Right a = parseOnly account . T.pack $ d
    pure a

-- NOTE(skylar): Found here https://docs.nervos.org/docs/basics/guides/devchain/
genesisPrivateKey1 :: T.Text
genesisPrivateKey1 = "0xd00c06bfd800d27397002dca6fb0993d5ba6399b4238b2f29ee9deb97593d2bc"

genesisPrivateKey2 :: T.Text
genesisPrivateKey2 = "0x63d86723e08f0f813a36ce6aa123bb2289d90680ae1e99d4de8cdb334553f24d"

initNewDevNode :: (MonadLog (WithSeverity T.Text) m, MonadIO m) => FilePath -> m DevNode
initNewDevNode path = do
  logDebug "Initializing new development chain"
  initDevNode path True

  minerAccount <- createNewAccount path

  r <- Toml.decodeFile ckbConfigCodec (path <> "/ckb.toml")
  Toml.encodeToFile ckbConfigCodec (path <> "/ckb.toml") $ r { block_assembler = Just $ mkBlockAssembler minerAccount }

  genesis1 <- accountFromPrivateKey path genesisPrivateKey1
  genesis2 <- accountFromPrivateKey path genesisPrivateKey2

  logDebug $ "Done, node created in " <> T.pack path

  let dn = DevNode minerAccount genesis1 genesis2
  liftIO $ Aeson.encodeFile (path <> "/dev-node.json") dn
  pure dn

-- TODO(skylar): Where should the logs go!
runDevNode :: (ForceM m) => FilePath -> m ()
runDevNode path = do
  liftIO $ createDirectoryIfMissing False path
  node <- getOrCreateDevNode path
  runChain path
  -- TODO(skylar): Properly poll/wait for the chain to start
  waitForChain
  runMiner path
  pure ()

runChain :: MonadIO m => FilePath -> m ()
runChain path = do
  _ <- liftIO $ createProcess $ inDirectory path $ proc ckbPath ["run"]
  pure ()

runMiner :: MonadIO m => FilePath -> m ()
runMiner path = do
  _ <- liftIO $ createProcess $ inDirectory path $ proc ckbPath ["miner"]
  pure ()

getOrCreateDevNode :: ForceM m => FilePath -> m DevNode
getOrCreateDevNode path = do
  exists <- liftIO $ doesFileExist file
  case exists of
    True -> do
      result <- liftIO $ Aeson.decodeFileStrict file
      case result of
        Nothing -> initNewDevNode path
        Just node -> pure node
    False -> do
      initNewDevNode path
  where
    file = path <> "/dev-node.json"

initDevNode :: MonadIO m => FilePath -> Bool -> m ()
initDevNode path force = do
  _ <- liftIO
    $ createProcess
    $ inDirectory path
    $ proc ckbPath ["init", "--chain", "dev", bool "" "--force" force]
  pure ()

-- TODO(skylar): How long does it take, or can we query this to ensure that things are good
waitForChain :: MonadIO m => m ()
waitForChain = liftIO $ threadDelay 1000000

waitForCapacity :: ForceM m => Account -> CKBytes -> m ()
waitForCapacity a amount = do
  curr <- getTestnetBalance a
  when (curr < amount) $ do
    liftIO $ threadDelay $ miliseconds 500
    waitForCapacity a amount

-- TODO(skylar): What is the failure state here?
-- TODO(skylar): This should be done via rpc
getTestnetBalance :: ForceM m => Account -> m CKBytes
getTestnetBalance ac = do
  let
    ta = testnet_address ac
    opts = [ "wallet"
           , "get-capacity"
           , "--output-format"
           , "json"
           , "--address"
           , T.unpack $ unTestnetAddress ta
           ]

  result <- liftIO $ readProcess ckbCliPath opts ""
  pure $ maybe (error "Failed to get balance") (total) $ Aeson.decode . LBS.fromStrict . BS.pack $ result
