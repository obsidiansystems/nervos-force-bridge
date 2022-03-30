{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 

module Bridge.Nervos.Cli where

import Data.Foldable

import System.IO.Temp
import System.Exit
import System.Which
import System.Directory
import System.Process

import Control.Lens

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T

import Data.Maybe

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Bridge.Utils
-- TODO Nervos types
import qualified Bridge.Cardano.Types as Ada
-- import qualified Bridge.Cardano as Ada
import Bridge.Nervos.Types

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

procCli :: MonadIO m => FilePath -> [String] -> m CreateProcess
procCli ckbCliDir args = liftIO $ do
  pure
    $ addEnvironmentVariable ("CKB_CLI_HOME", ckbCliDir)
    $ proc ckbCliPath args

procWithCkbCliIn :: MonadIO m => FilePath -> FilePath -> FilePath -> [String] -> m CreateProcess
procWithCkbCliIn ckbCliDir wd path args = liftIO $ do
  absoluteDir <- makeAbsolute ckbCliDir
  pure
    $ addEnvironmentVariable ("CKB_CLI_HOME", absoluteDir)
    $ inDirectory wd
    $ proc path args

addEnvironmentVariable :: (String, String) -> CreateProcess -> CreateProcess
addEnvironmentVariable = addEnvironmentVariables . pure

addEnvironmentVariables :: [(String, String)] -> CreateProcess -> CreateProcess
addEnvironmentVariables args cp =
  cp { env = env cp <> Just args }

relativeCkbHome :: FilePath -> FilePath
relativeCkbHome = (<> "/.ckb-cli")

inDirectory :: FilePath -> CreateProcess -> CreateProcess
inDirectory fp cp = cp { cwd = Just fp }

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

instance ToJSON MultiSigConfigs where
  toJSON = const $ object []

instance FromJSON MultiSigConfigs where
  parseJSON = withObject "MultiSigConfigs" $ const (pure MultiSigConfigs)

data Signatures =
  Signatures {}
  deriving (Eq, Show)

instance ToJSON Signatures where
  toJSON = const $ object []

instance FromJSON Signatures where
  parseJSON = withObject "Signatures" $ const (pure Signatures)

data TxFile = TxFile
  { _txFile_transaction :: Txn
  , _txFile_multisig_configs :: MultiSigConfigs
  , _txFile_signatures :: Signatures
  }
  deriving (Eq, Show)

data CliConfig =
  CliConfig { cliConfigHome :: Maybe FilePath
            }

data AddressInfo =
  AddressInfo { addressInfo_lock_script :: Script
              }

makeLenses ''TxFile
makeLenses ''Txn

deriveJSON (scrubPrefix "input_") ''Input
deriveJSON (scrubPrefix "output_") ''Output
-- deriveJSON (scrubPrefix "depType_") ''DepType
deriveJSON (scrubPrefix "outPoint_") ''OutPoint
deriveJSON (scrubPrefix "cellDep_") ''CellDep
deriveJSON (scrubPrefix "_tx_") ''Txn
deriveJSON (scrubPrefix "_txFile_") ''TxFile
deriveJSON (scrubPrefix "liveCell_") ''LiveCell
deriveJSON (scrubPrefix "liveCells_") ''LiveCells
deriveJSON (scrubPrefix "addressInfo_") ''AddressInfo

getAddressInfo :: BridgeM m => Address -> m Script
getAddressInfo (Address addr) = do
  let
    opts = [ "util"
           , "address-info"
           , "--address"
           , T.unpack addr
           , "--output-format"
           , "json"
           ]
  result <- liftIO $ readProcess ckbCliPath opts ""
  pure $ maybe (error "Failed to decode address") (addressInfo_lock_script) $ decode . LBS.fromStrict . BS.pack $ result

getLiveCells :: BridgeM m => Address -> m LiveCells
getLiveCells (Address ta) = do
  let
    opts = [ "wallet"
           , "get-live-cells"
           , "--address"
           , T.unpack ta
           , "--output-format"
           , "json"
           ]
  result <- liftIO $ readProcess ckbCliPath opts ""
  pure $ maybe (error "Failed to fetch/decode live cells") (id) $ decode . LBS.fromStrict . BS.pack $ result

-- | Coin selection is the process of selecting which inputs we need to fund the transaction
coinSelection :: BridgeM m => Address -> CKBytes -> m [LiveCell]
coinSelection addr amount = do
  -- TODO actual coin selection
  -- TODO Deterministic coin selection
  cells <- liveCells_live_cells <$> getLiveCells addr
  pure $ go cells amount
  where
    go :: [LiveCell] -> CKBytes -> [LiveCell]
    go (c:cs) left
      | left <= (shannons 0) = []
      | otherwise = [c] ++ go cs (diffCkb left (liveCell_capacity c))

addChangeOutput :: BridgeM m => FilePath -> Address -> CKBytes -> CKBytes -> m ()
addChangeOutput file (Address toAddr) ckbytes fee = do
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

  -- logError $ "Amount: " <> (T.pack . show $ ckbytes)
  -- logError $ "Fee: " <> (T.pack . show $ fee)
  -- logError $ "CHANGE IS: " <> (T.pack . show $ f)
  result <- liftIO $ readProcess ckbCliPath opts ""
  -- undefined
  -- logError $ "Add change result: " <> T.pack result
  pure ()
  where
    f = ckbytesToDouble $ diffCkb ckbytes fee

addInput :: BridgeM m => FilePath -> LiveCell -> m ()
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

-- TODO (ckb cli config)
addSignature :: BridgeM m => FilePath -> Address -> T.Text -> m ()
addSignature file (Address addr) pass = do
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
  -- logError $ "TEHE: " <> T.pack result
  pure ()

buildMintTxn :: BridgeM m => Address -> DeployedScript -> Ada.LockTx -> m FilePath
buildMintTxn addr (DeployedScript sudt sudtDep) (Ada.LockTx h s lovelace) = do
  (fname, _) <- liftIO $ openTempFile "." "tx.json"
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

    totalCapacity = foldr (addCkb . liveCell_capacity) (shannons 0) coins

  liftIO $ encodeFile fname txFile
  for_ coins (addInput fname)
  addChangeOutput fname addr (diffCkb totalCapacity cellCost) (ckb 0.0001)

  mTx <- liftIO $ decodeFileStrict fname
  case mTx of
    Nothing -> do
      pure ()
    Just tx' -> do
      let tx'' =
            tx' & txFile_transaction . tx_outputs %~ (mintOutput :)
                & txFile_transaction . tx_outputs_data %~ (mintOutputData :)
                & txFile_transaction . tx_cell_deps %~ (sudtDep :)

      -- liftIO $ putStrLn "YABBA DABBA DO"
      liftIO $ encodeFile fname tx''

  -- TODO close the file
  pure fname
  where
    fee = ckb 0.0001

    mintOutput =
      Output
      "0x22ecb25c000"
      s
      (Just sudt)

    -- TODO(skylar): Make this amount a proper amount
    mintOutputData = "0xe80300000000000000000000000000"

    cellCost = ckb 24000

submitTxFromFile :: BridgeM m => FilePath -> m (Maybe T.Text)
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

emptyTxn :: Txn
emptyTxn =
  Txn
  "0x0"
  []
  []
  []
  []
  []
  []

emptyTxFile :: TxFile
emptyTxFile =
  TxFile
  emptyTxn
  MultiSigConfigs
  Signatures
