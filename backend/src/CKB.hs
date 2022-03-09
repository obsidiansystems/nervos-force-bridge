{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: CKB node running utilities
-}

module CKB ( runDevNode
           , waitForCapacity
           , getTestnetBalance
           ) where

import Control.Monad
import Control.Monad.IO.Class

import GHC.Generics

import System.Process
import System.Directory

import Data.Bool
import Data.List (intercalate)

import GHC.IO.Handle

import System.IO.Temp
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson
import Data.Aeson.TH

import Control.Concurrent
import Control.Monad.Log

import qualified Toml.Codec as Toml

import CKB.Types
import CKB.Config

import Data.Attoparsec.Text as A

import Backend.Utils

-- Password for dev node is "hello"
data DevNode = DevNode
  { devNodeMiner :: Account
  , devNodeGenesis1 :: Account
  , devNodeGenesis2 :: Account
  }
  deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''DevNode

createNewAccount :: MonadIO m => FilePath -> m Account
createNewAccount path = liftIO $ do
  cp <- procCli (relativeCkbHome path) ["account", "new"]
  d <- readCreateProcess cp input
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
    cp <- procCli (relativeCkbHome path) ["account"
                                         , "import"
                                         , "--privkey-path"
                                         , fp
                                         ]
    d <- readCreateProcess cp "hello\nhello"
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
  _ <- Toml.encodeToFile ckbConfigCodec (path <> "/ckb.toml") $ r { block_assembler = Just $ mkBlockAssembler minerAccount }

  genesis1 <- accountFromPrivateKey path genesisPrivateKey1
  genesis2 <- accountFromPrivateKey path genesisPrivateKey2

  logDebug $ "Done, node created in " <> T.pack path

  let dn = DevNode minerAccount genesis1 genesis2
  liftIO $ Aeson.encodeFile (path <> "/dev-node.json") dn
  pure dn

runDevNode :: (ForceM m) => FilePath -> m ()
runDevNode path = do
  liftIO $ createDirectoryIfMissing False path
  _ <- getOrCreateDevNode path
  runChain path
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
  exists <- liftIO $ doesFileExist nodeFile
  case exists of
    True -> do
      result <- liftIO $ Aeson.decodeFileStrict nodeFile
      case result of
        Nothing -> initNewDevNode path
        Just node -> pure node
    False -> do
      initNewDevNode path
  where
    nodeFile = path <> "/dev-node.json"

initDevNode :: MonadIO m => FilePath -> Bool -> m ()
initDevNode path force = do
  _ <- liftIO
    $ createProcess
    $ inDirectory path
    $ proc ckbPath ["init", "--chain", "dev", bool "" "--force" force]
  pure ()

waitForChain :: MonadIO m => m ()
waitForChain = liftIO $ threadDelay 1000000

waitForCapacity :: ForceM m => Account -> CKBytes -> m ()
waitForCapacity a amount = do
  curr <- getTestnetBalance a
  when (curr < amount) $ do
    liftIO $ threadDelay $ miliseconds 500
    waitForCapacity a amount

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
