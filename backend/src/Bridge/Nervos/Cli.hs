{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | 

module Bridge.Nervos.Cli where

import Data.Foldable

import System.IO (hClose)
import System.IO.Temp
import System.Exit
import System.Which
import System.Directory
import System.Process

import Network.Web3.Provider

import Control.Lens

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.HexString as HS

import Data.Maybe
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

import Bridge.Utils
-- TODO Nervos types
import qualified Bridge.Cardano.Types as Ada
-- import qualified Bridge.Cardano as Ada
import Bridge.Nervos.Types
import Bridge.Nervos.SUDT

-- TODO(skylar): We aren't really using cli for these calls,
-- we just import this to make it work for now
import qualified Bridge.Nervos.RPC as RPC

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

-- TODO(skylar): Do not have this at all, or make it consistent
procCli :: MonadIO m => FilePath -> [String] -> m CreateProcess
procCli ckbCliDir args = liftIO $ do
  pure
    -- $ addEnvironmentVariable ("CKB_CLI_HOME", ckbCliDir)
    $ proc ckbCliPath args

{-
procWithCkbCliIn :: MonadIO m => FilePath -> FilePath -> FilePath -> [String] -> m CreateProcess
procWithCkbCliIn ckbCliDir wd path args = liftIO $ do
  absoluteDir <- makeAbsolute ckbCliDir
  pure
    -- $ addEnvironmentVariable ("CKB_CLI_HOME", absoluteDir)
    $ inDirectory wd
    $ proc path args
-}

addEnvironmentVariable :: (String, String) -> CreateProcess -> CreateProcess
addEnvironmentVariable = addEnvironmentVariables . pure

addEnvironmentVariables :: [(String, String)] -> CreateProcess -> CreateProcess
addEnvironmentVariables args cp =
  cp { env = env cp <> Just args }

relativeCkbHome :: FilePath -> FilePath
relativeCkbHome = (<> "/.ckb-cli")

inDirectory :: FilePath -> CreateProcess -> CreateProcess
inDirectory fp cp = cp { cwd = Just fp }

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

data MultiSigConfigs = MultiSigConfigs (M.Map LockArg MultiSigConfig)
  deriving (Eq, Show)

data MultiSigConfig = MultiSigConfig
  { sighash_addresses :: [Address] -- ckb addresses
  , require_first_n :: Int
  , threshold :: Int
  } deriving (Eq, Show)

-- instance ToJSON MultiSigConfigs where
--   toJSON = const $ object []

-- instance FromJSON MultiSigConfigs where
--   parseJSON = withObject "MultiSigConfigs" $ const (pure MultiSigConfigs)

data Signatures = Signatures (M.Map LockArg [Signature])
  deriving (Eq, Show)


newtype Signature =
  Signature { unSignature :: T.Text }
  deriving (Eq, Show)

type LockArg = T.Text

-- instance ToJSON Signatures where
--   toJSON = const $ object []

-- instance FromJSON Signatures where
--   parseJSON = withObject "signatures" $ const (pure Signatures)


-- Whats known about Signatures and multisig configs
  -- they are built around a particular lock-arg which comes from the multisig address
  -- require 




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

instance ToJSON Signature where
  toJSON (Signature t) = String t

instance FromJSON Signature where
  parseJSON = withText "Signature" (fmap Signature . pure)

deriveJSON (scrubPrefix "input_") ''Input
deriveJSON (scrubPrefix "output_") ''Output
-- deriveJSON (scrubPrefix "depType_") ''DepType
deriveJSON (scrubPrefix "outPoint_") ''OutPoint
deriveJSON (scrubPrefix "cellDep_") ''CellDep
deriveJSON (scrubPrefix "_tx_") ''Txn
deriveJSON (scrubPrefix "_txFile_") ''TxFile
deriveJSON (scrubPrefix "addressInfo_") ''AddressInfo
-- deriveJSON defaultOptions ''Signature
deriveJSON defaultOptions ''Signatures
deriveJSON defaultOptions ''MultiSigConfig
deriveJSON defaultOptions ''MultiSigConfigs

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

ckbIndexerProvider :: Provider
ckbIndexerProvider =
  HttpProvider "http://obsidian.webhop.org:9116"

getLiveCells :: BridgeM m => Script -> m [LiveCell]
getLiveCells scr = do
  {-
  resultValue <- RPC.runIndexer ckbIndexerProvider (RPC.getLiveCells' searchKey RPC.Desc "0x64")
  case resultValue of
    Left _ -> pure ()
    Right thing ->
      liftIO $ encodeFile "get_cells.json" thing
-}
  result <- fmap getCellsResult_objects <$> RPC.runIndexer ckbIndexerProvider (RPC.getLiveCells searchKey RPC.Desc "0x64")
  case result of
    Left _ -> pure []
    Right ls -> pure ls
  where
    searchKey = RPC.SearchKey scr RPC.Lock

-- TODO IMPORTANT Make part of config
verifiersMultiSig :: Script
verifiersMultiSig =
  Script
  "0x5c5069eb0857efc65e1bca0c07df34c31663b3622fd3876c876320fc9634e2a8"
  HashTypeType
  "0xc099a986b4c66a590b09011e3b139bf5a73e2e50"

-- | Coin selection is the process of selecting which inputs we need to fund the transaction
coinSelection :: BridgeM m => Script -> CKBytes -> m [LiveCell]
coinSelection script amount = do
  logDebug "Starting coin selection"
  -- TODO actual coin selection
  -- TODO Deterministic coin selection
  cells <- getLiveCells script
  pure $ go cells amount
  where
    go :: [LiveCell] -> CKBytes -> [LiveCell]
    go (c:cs) left
      | left <= (shannons 0) = []
      | otherwise = [c] ++ go cs (diffCkb left (liveCell_capacity c))
    go _ _ = []

{-
tx add-output --to-long-multisig-address ckt1qyqupxdfs66vv6jepvysz83mzwdltfe79egqa4geg5 --capacity 3000 --tx-file blank.json
-}

-- TODO(skylar): Make this address configurable, or only take multisig addresses
addChangeOutput :: BridgeM m => FilePath -> Address -> CKBytes -> CKBytes -> m ()
addChangeOutput file (Address toAddr) ckbytes fee = do
  logDebug "Adding change"
  let
    opts = [ "tx"
           , "add-output"
           , "--to-short-multisig-address"
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
  logDebug "Adding input"
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
  pure ()

-- More generally, when we sign, we sign *for* a given lock arg - so to put a signature we either
-- need to have created or create it, by
  -- calling the CLI with our lock-arg of the account (or multisig account) which we would like to
  --    transfer from
  -- OR
  -- calling the CLI 


-- the multisig basically translates to for this lock-arg (pertaining to the msig address)
-- we require some amount of these following addresses


-- TODO (ckb cli config)
-- TODO Make the relativeckbhome configurable or just assume the global one (which I don't wanna do)
signTxFile :: BridgeM m => FilePath -> Address -> T.Text -> m ()
signTxFile file (Address addr) pass = do
  logDebug "Signing tx file"
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

buildMintTxn :: BridgeM m =>
                Address
             -> Script
             -> MultiSigConfigs
                -- ^ CKB Multisig address 
             -> DeployedScript
             -> Ada.LockTx
             -> m FilePath
buildMintTxn addr script msconfig (DeployedScript sudt sudtDep) (Ada.LockTx h s lovelace) = do
  (fname, handle) <- liftIO $ openTempFile "." "tx.json"
  liftIO $ hClose handle
  logDebug $ "Building a mint tx" <> T.pack fname
  coins <- coinSelection script cellCost
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
      msconfig
      (Signatures mempty)

    totalCapacity = foldr (addCkb . liveCell_capacity) (shannons 0) coins

  liftIO $ do
    encodeFile fname txFile

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

      liftIO $ encodeFile fname tx''

  pure fname
  where
    fee = ckb 0.0001

    mintOutput =
      Output
      "0x9502f9000"
      s
      (Just sudt)

    mintOutputData = "0x" <> (HS.toText . HS.fromBinary $ SUDTAmount $ fromIntegral lovelace)

    cellCost = ckb 400

-- tx add-multisig-config --sighash-address ckt1qyqw2mw2tx493vhtcf5g7rzxggldfxtvn2ksdheprt ckt1qyqxak478atwzfx0kqqa4sepnfqfgd7x2kesjy0v6k ckt1qyq0222yxth2mtj3jmyt9uzkfxkrf4yehtjs5xvgnk --threshold 2 --tx-file

submitTxFromFile :: BridgeM m => FilePath -> m (Maybe T.Text)
submitTxFromFile file = do
  logDebug "Submitting tx"
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
  (MultiSigConfigs mempty)
  (Signatures mempty) 

-- | Used to see if the tx json matches internal haskell type
parseAndWriteTxFile :: FilePath -> FilePath -> IO ()
parseAndWriteTxFile input output = do
  result :: Either String TxFile <- eitherDecodeFileStrict input
  case result of
    Left err -> putStrLn err
    Right thing -> do
      encodeFile output thing
