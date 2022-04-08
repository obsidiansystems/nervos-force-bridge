{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: CKB node running utilities
-}

module CKB ( runDevNode
           , waitForCapacity
           , getTestnetBalance
           , waitForChain
           , decodeAddress

           , getLiveCells
           , coinSelection

           , buildMintTxFile

           , LockTx(..)
           , TxHash(..)
           , CellDep(..)
           ) where

import System.Exit
import Control.Lens

import Data.Foldable
import CKB.Utils
import CKB.RPC
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.HexString as HS

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
import Data.Aeson ((.:))
import Data.Aeson.TH

import Control.Concurrent
import Control.Monad.Log

import qualified Toml.Codec as Toml

import CKB.Types
import CKB.Config

import Data.Attoparsec.Text as A

import Backend.Utils

data LockTx =
  LockTx { lockTxHash :: TxHash
         , lockTxToAddress :: Script
         , lockTxLovelace :: Integer -- Lovelace
         }
  deriving (Show)

newtype TxHash = TxHash
  { unHash :: T.Text }
  deriving (Show)

instance Aeson.FromJSON TxHash where
  parseJSON = Aeson.withObject "TxHash" $ \o ->
    TxHash <$> o .: "tx_hash"

-- Password for dev node is "hello"
data DevNode = DevNode
  { devNodeMiner :: Account
  , devNodeGenesis1 :: Account
  , devNodeGenesis2 :: Account
  }
  deriving (Eq, Show, Generic)

data LiveCell = LiveCell
  { liveCell_capacity :: CKBytes
  , liveCell_tx_hash :: T.Text
  , liveCell_output_index :: Int
  }
  deriving (Eq, Show)

data LiveCells = LiveCells
  { liveCells_total_capacity :: CKBytes
  , liveCells_total_count :: Int
  , liveCells_live_cells :: [LiveCell]
  }
  deriving (Eq, Show)

data Input = Input
  { input_since :: T.Text
  , input_previous_output :: OutPoint
  }
  deriving (Eq, Show)

data Output = Output
  { output_capacity :: T.Text
  , output_lock :: Script
  , output_type :: Maybe Script
  }
  deriving (Eq, Show)

data Txn = Txn
  { _tx_version :: T.Text
  , _tx_cell_deps :: [CellDep]
  , _tx_header_deps :: [CellDep]
  , _tx_inputs :: [Input]
  , _tx_outputs :: [Output]
  , _tx_outputs_data :: [T.Text]
  , _tx_witnesses :: [T.Text]
  }
  deriving (Eq, Show)

data MultiSigConfigs =
  MultiSigConfigs {}
  deriving (Eq, Show)

instance Aeson.ToJSON MultiSigConfigs where
  toJSON = const $ Aeson.object []

instance Aeson.FromJSON MultiSigConfigs where
  parseJSON = Aeson.withObject "MultiSigConfigs" $ const (pure MultiSigConfigs)

data Signatures =
  Signatures {}
  deriving (Eq, Show)

instance Aeson.ToJSON Signatures where
  toJSON = const $ Aeson.object []

instance Aeson.FromJSON Signatures where
  parseJSON = Aeson.withObject "Signatures" $ const (pure Signatures)

data TxFile = TxFile
  { _txFile_transaction :: Txn
  , _txFile_multisig_configs :: MultiSigConfigs
  , _txFile_signatures :: Signatures
  }
  deriving (Eq, Show)

makeLenses ''TxFile
makeLenses ''Txn

-- TODO Can we just auto populate this prefix without hardcoding it
-- deriveJSON (scrubPrefix "tx_") ''Tx
deriveJSON (scrubPrefix "input_") ''Input
deriveJSON (scrubPrefix "output_") ''Output
-- deriveJSON (scrubPrefix "depType_") ''DepType
deriveJSON (scrubPrefix "outPoint_") ''OutPoint
deriveJSON (scrubPrefix "cellDep_") ''CellDep
deriveJSON (scrubPrefix "_tx_") ''Txn
deriveJSON (scrubPrefix "_txFile_") ''TxFile
deriveJSON (scrubPrefix "liveCell_") ''LiveCell
deriveJSON (scrubPrefix "liveCells_") ''LiveCells

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

-- TODO(skylar): Don't show the logs plox
runDevNode :: (ForceM m) => FilePath -> m ()
runDevNode path = do
  liftIO $ createDirectoryIfMissing False path
  _ <- getOrCreateDevNode path
  runChain path
  waitForChain
  runMiner path
  waitForChain
  runIndexer path
  pure ()

runIndexer :: MonadIO m => FilePath -> m ()
runIndexer path = liftIO $ do
  _ <- createProcess $ proc ckbIndexerPath ["-s"
                                           , indexerPath
                                           ]
  pure ()
  where
    indexerPath = path <> "/indexer"

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

-- TODO(skylar): Might fail tho...
-- TODO(skylar): Make this take the address, cause then it really _can't_ fail...
decodeAddress :: ForceM m => T.Text -> m Script
decodeAddress ta = do
  let
    opts = [ "util"
           , "address-info"
           , "--address"
           , T.unpack ta
           , "--output-format"
           , "json"
           ]

  result <- liftIO $ readProcess ckbCliPath opts ""
  pure $ maybe (error "Failed to decode address") (lock_script) $ Aeson.decode . LBS.fromStrict . BS.pack $ result

{-
    {
      "capacity": "2009.88413529 (CKB)",
      "data_bytes": 0,
      "index": {
        "output_index": 0,
        "tx_index": 0
      },
      "lock_hash": "0x13146ce73ad549724291df1ecb476c6cc5837a9a1e5393728be71cba9b885027",
      "mature": true,
      "number": 26,
      "output_index": 0,
      "tx_hash": "0x7c336e0e78a152bb2eda09a254d4cb8bef57e933674e1a3122a2041342bd8c73",
      "type_hashes": null
    }
-}

getLiveCells :: ForceM m => T.Text -> m LiveCells
getLiveCells ta = do
  let
    opts = [ "wallet"
           , "get-live-cells"
           , "--address"
           , T.unpack ta
           , "--output-format"
           , "json"
           ]

  result <- liftIO $ readProcess ckbCliPath opts ""
  pure $ maybe (error "Failed to fetch/decode live cells") (id) $ Aeson.decode . LBS.fromStrict . BS.pack $ result

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

-- TODO(skylar): Don't spend the SUDTs
coinSelection :: ForceM m => T.Text -> CKBytes -> m [LiveCell]
coinSelection addr amount = do
  -- TODO actual coin selection
  cells <- liveCells_live_cells <$> getLiveCells addr
  pure $ go cells amount
  where
    go :: [LiveCell] -> CKBytes -> [LiveCell]
    go (c:cs) left
      | left <= (shannons 0) = []
      | otherwise = [c] ++ go cs (diffCkb left (liveCell_capacity c))

addInput :: ForceM m => FilePath -> LiveCell -> m ()
addInput file (LiveCell _ hash index) = do
  let
    opts = [ "tx"
           , "add-input"
           , "--tx-hash"
           , T.unpack hash
           , "--index"
           , show index
           , "--tx-file"
           , file
           , "--output-format"
           , "json"
           ]

  result <- liftIO $ readProcess ckbCliPath opts ""
  logInfo $ "Result: " <> T.pack result
  pure ()

-- signTransaction :: T.
addChangeOutput :: ForceM m => FilePath -> T.Text -> CKBytes -> CKBytes -> m ()
addChangeOutput file toAddr ckbytes fee = do
  let
    opts = [ "tx"
           , "add-output"
           , "--to-sighash-address"
           , T.unpack toAddr
           , "--capacity"
           , show f
           , "--tx-file"
           , file
           , "--output-format"
           , "json"
           ]

  _ <- liftIO $ readProcess ckbCliPath opts ""
  pure ()
  where
    f = ckbytesToDouble $ diffCkb ckbytes fee

addSignature :: ForceM m => FilePath -> T.Text -> T.Text -> m ()
addSignature file addr pass = do
  let
    opts = [ "tx"
           , "sign-inputs"
           , "--from-account"
           , T.unpack addr
           , "--tx-file"
           , file
           , "--add-signatures"
           , "--output-format"
           , "json"
           ]
  cp <- procCli (relativeCkbHome "ckb") opts
  result <- liftIO $ readCreateProcess cp (T.unpack pass)
  pure ()

submitTxFromFile :: ForceM m => FilePath -> m (Maybe T.Text)
submitTxFromFile file = do
  let
    opts = [ "tx"
           , "send"
           , "--tx-file"
           , file
           ]
  cp <- procCli (relativeCkbHome "ckb") opts
  (ec, result, _) <- liftIO $ readCreateProcessWithExitCode cp ""
  pure $ case ec of
    ExitSuccess -> Just $ T.pack result
    _ -> Nothing

-- TODO Make Script have wrappers that are Lock and Type just for type safety
buildMintTxFile :: ForceM m => LockTx -> m TxFile
buildMintTxFile (LockTx h s lovelace) = do
  coins <- coinSelection addr cellCost
  let
    tx =
      Txn
      "0x0"
      []
      []
      []
      []
      []
      []

    txFile =
      TxFile
      tx
      MultiSigConfigs
      Signatures

    fname = "testtx.json"

    totalCapacity = foldr (addCkb . liveCell_capacity) (shannons 0) coins

  liftIO $ Aeson.encodeFile fname txFile
  for_ coins (addInput fname)
  addChangeOutput fname addr (diffCkb totalCapacity cellCost) (ckb 0.0001)

  mTx <- liftIO $ Aeson.decodeFileStrict fname
  case mTx of
    Nothing -> do
      pure ()
    Just tx' -> do
      let tx'' =
            tx' & txFile_transaction . tx_outputs %~ (mintOutput :)
                & txFile_transaction . tx_outputs_data %~ (mintOutputData :)
                & txFile_transaction . tx_cell_deps %~ (mintDep :)
      liftIO $ Aeson.encodeFile fname tx''

  pure txFile
  where
    addr =
      "ckt1qyq8d80l0z2y40hwkjjhkc68yv48gau2dwlsvzucru"

    fee = ckb 0.0001

    mintDep =
      deployedSUDTDep

    mintOutput =
      Output
      "0x22ecb25c000"
      s
      (Just deployedSUDT)

    mintOutputData = "0x" <> (HS.toText . HS.fromBinary $ SUDTAmount $ fromIntegral lovelace)

    cellCost = ckb 24000
