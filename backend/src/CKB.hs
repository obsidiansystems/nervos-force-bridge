{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CKB (runDevelopmentChain) where

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

import System.IO.Temp
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as Aeson

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
  deriving (Eq, Show)

-- We pipe in a temporary file as Stdin to be able to provide a password to the create account utility
createNewAccount :: MonadIO m => m Account
createNewAccount = liftIO $ do
  d <- readProcess ckbCliPath ["account", "new"] input
  let Done _ a = parse account . T.pack $ d
  pure a
  where
    pass = "hello"
    input = intercalate "\n" [pass, pass]

-- TODO(skylar): This fails if certain files already exist
-- Force doesn't actually delete all the things it should :(
-- TODO(skylar): Naming
runDevChain :: (MonadLog (WithSeverity T.Text) m, MonadIO m) => FilePath -> Bool -> m DevNode
runDevChain path force = do
  forceInitChain
  minerAccount <- createNewAccount
  r <- Toml.decodeFile ckbConfigCodec "ckb/ckb.toml"
  Toml.encodeToFile ckbConfigCodec "ckb/ckb.toml" $ r { block_assembler = Just $ mkBlockAssembler minerAccount }
  logInfo "Running chain"
  runChain
  waitForChain
  logInfo "Running miner"
  runMiner
  genesis1 <- accountFromPrivateKey genesisPrivateKey1
  genesis2 <- accountFromPrivateKey genesisPrivateKey2
  pure $ DevNode minerAccount genesis1 genesis2
  where
    runChain = liftIO $ createProcess $ inDirectory path $ proc ckbPath ["run"]
    runMiner = liftIO $ createProcess $ inDirectory path $ proc ckbPath ["miner"]
    forceInitChain =
      liftIO
      $ createProcess
      $ inDirectory path
      $ proc ckbPath ["init", "--chain", "dev", bool "" "--force" force]

-- TODO(skylar): How long does it take, or can we query this to ensure that things are good
waitForChain :: MonadIO m => m ()
waitForChain = liftIO $ threadDelay 1000000

-- NOTE(skylar): Found here https://docs.nervos.org/docs/basics/guides/devchain/
genesisPrivateKey1 :: T.Text
genesisPrivateKey1 = "0xd00c06bfd800d27397002dca6fb0993d5ba6399b4238b2f29ee9deb97593d2bc"

genesisPrivateKey2 :: T.Text
genesisPrivateKey2 = "0x63d86723e08f0f813a36ce6aa123bb2289d90680ae1e99d4de8cdb334553f24d"

accountFromPrivateKey :: ForceM m => T.Text -> m Account
accountFromPrivateKey pk = do
  liftIO $ withTempFile "." "pk" $ \fp handle -> do
    T.hPutStr handle pk
    hFlush handle
    d <- readProcess ckbCliPath ["account", "import", "--privkey-path", fp] ""
    let Done _ a = parse account . T.pack $ d
    pure a

runDevelopmentChain :: (ForceM m) => FilePath -> m DevNode
runDevelopmentChain directory = do
  liftIO $ do
    exists <- doesDirectoryExist directory
    case exists of
      False -> pure ()
      True -> removePathForcibly directory
    createDirectoryIfMissing False directory
  dn <- runDevChain directory True
  let
    miner = devNodeMiner dn
    genesis1 = devNodeGenesis1 dn
  waitForChain
  deployProject genesis1 "on-chain/ckb/"
  -- waitForCapacity (devNodeMiner dn) 42000
  -- acc <- getTestnetBalance miner
  -- logInfo $ "Balance for miner: " <> tShow acc
  -- deployProject miner "on-chain/ckb/"
  pure dn

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
