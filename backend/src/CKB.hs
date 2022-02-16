{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CKB (runDevelopmentChain) where

import Control.Monad.IO.Class

import GHC.Generics

import System.Which
import System.Process
import System.Directory

import Data.Bool
import Data.List (intercalate)

import Toml.Codec ( TomlCodec
                  , HasCodec
                  , genericCodec
                  )
import qualified Toml.Codec as Toml

import GHC.IO.Handle
import Obelisk.Configs
import qualified Obelisk.ExecutableConfig.Lookup as Lookup

import System.IO.Temp
import qualified Data.Text as T

import Data.Attoparsec.Text as A

data CkbConfig =
  CkbConfig { data_dir :: T.Text
            , chain :: ChainSpec
            , logger :: Logger
            , sentry :: Sentry
            , db :: Db
            , network :: Network
            , rpc :: RPC
            , tx_pool :: TxPool
            , store :: Store
            , block_assembler :: Maybe BlockAssembler
            }
  deriving (Generic, Show)

newtype SpecFile =
  SpecFile { file :: T.Text
           }
  deriving (Generic, Show)

data ChainSpec =
  ChainSpec { spec :: SpecFile
            }
  deriving (Generic, Show)

data Logger =
  Logger { filter :: T.Text -- TODO(skylar): This should be an enum probably
         , color :: Bool
         , log_to_file :: Bool
         , log_to_stdout :: Bool
         }
  deriving (Generic, Show)

data Sentry =
  Sentry { dsn :: T.Text
         }
  deriving (Generic, Show)

data Db =
  Db { cache_size :: Integer
     }
  deriving (Generic, Show)

data Network =
  Network { listen_addresses :: [T.Text]
          , bootnodes :: [T.Text]
          , max_peers :: Int
          , max_outbound_peers :: Int
          , ping_interval_secs :: Int
          , ping_timeout_secs :: Int
          , connect_outbound_interval_secs :: Int
          , upnp :: Bool
          , discovery_local_address :: Bool
          , bootnode_mode :: Bool
          }
  deriving (Generic, Show)

data RPC =
  RPC { listen_address :: T.Text
      , max_request_body_size :: Int
      , modules :: [T.Text]
      , reject_ill_transactions :: Bool
      , enable_deprecated_rpc :: Bool
      }
  deriving (Generic, Show)

data TxPool =
  TxPool { max_mem_size :: Int
         , max_cycles :: Int
         , max_verify_cache_size :: Int
         , max_conflict_cache_size :: Int
         , max_committed_txs_hash_cache_size :: Int
         , min_fee_rate :: Int
         , max_tx_verify_cycles :: Int
         , max_ancestors_count :: Int
         }
  deriving (Generic, Show)

data Store =
  Store { header_cache_size :: Int
        , cell_data_cache_size :: Int
        , block_proposals_cache_size :: Int
        , block_tx_hashes_cache_size :: Int
        , block_uncles_cache_size :: Int
        , cellbase_cache_size :: Int
        }
  deriving (Generic, Show)

data BlockAssembler = BlockAssembler
  { code_hash :: T.Text
  , args :: T.Text
  , hash_type :: T.Text
  , message :: T.Text
  }
  deriving (Generic, Show)

mkBlockAssembler :: Account -> BlockAssembler
mkBlockAssembler a =
  BlockAssembler
  magicHash
  (lock_arg a)
  alwaysHashType
  -- TODO(skylar): What is message for?
  "0x"
  where
    magicHash = "0x9bd7e06f3ecf4be0f2fcd2188b23f1b9fcc88e5d4b65a8637b17723bbda3cce8"
    alwaysHashType = "type"

ckbConfigCodec :: TomlCodec CkbConfig
ckbConfigCodec = genericCodec

-- TODO(skylar): This should really be an inline table, but inline tables are just alternative syntax
instance HasCodec ChainSpec where
  hasCodec = Toml.table genericCodec

instance HasCodec TxPool where
  hasCodec = Toml.table genericCodec

instance HasCodec Store where
  hasCodec = Toml.table genericCodec

instance HasCodec RPC where
  hasCodec = Toml.table genericCodec

instance HasCodec BlockAssembler where
  hasCodec = Toml.table genericCodec

instance HasCodec SpecFile where
  hasCodec = Toml.table genericCodec

instance HasCodec Logger where
  hasCodec = Toml.table genericCodec

instance HasCodec Sentry where
  hasCodec = Toml.table genericCodec

instance HasCodec Db where
  hasCodec = Toml.table genericCodec

instance HasCodec Network where
  hasCodec = Toml.table genericCodec

data Account =
  Account { mainnet_address :: Address
          , testnet_address :: Address
          -- TODO(skylar): Typing for this?
          , lock_arg :: T.Text
          , lock_hash :: T.Text
          }
  deriving (Show)

newtype Address =
  Address T.Text
  deriving (Eq, Show)

account :: Parser Account
account = do
  Account
    <$> (Address <$> mainnet)
    <*> (Address <$> testnet)
    <*> addr "lock_arg"
    <*> addr "lock_hash"
  where
    addr t = do
      A.takeTill isEndOfLine
      endOfLine
      skipMany space
      string $ t <> ":"
      skipSpace
      takeTill isEndOfLine

    mainnet = addr "mainnet"
    testnet = addr "testnet"

-- We pipe in a temporary file as Stdin to be able to provide a password to the create account utility
createNewAccount :: MonadIO m => m Account
createNewAccount = liftIO $ do
  d <- readProcess ckbCliPath ["account", "new"] input
  let Done _ a = parse account . T.pack $ d
  pure a
  where
    pass = "hello"
    input = intercalate "\n" [pass, pass]

ckbPath :: FilePath
ckbPath = $(staticWhich "ckb")

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

-- TODO(skylar): This fails if certain files already exist
-- Force doesn't actually delete all the things it should :(
initDevChain :: (MonadIO m) => FilePath -> Bool -> m ()
initDevChain path force = liftIO $ do
  -- cfgs <- Lookup.getConfigs
  -- Just pass <- runConfigsT cfgs $ getConfig "backend/password"
  -- print pass
  _ <- createProcess cp
  account <- createNewAccount
  r <- Toml.decodeFile ckbConfigCodec "ckb/ckb.toml"
  Toml.encodeToFile ckbConfigCodec "ckb/ckb.toml" $ r { block_assembler = Just $ mkBlockAssembler account }
  _ <- createProcess cp'
  pure ()
  where
    -- TODO(skylar): This can just be a shell or proc instead of doing this by hand
    cp' =
      CreateProcess
      (ShellCommand $ intercalate " " [ckbPath, "run"])
      (Just path)
      Nothing
      Inherit
      Inherit
      Inherit
      False
      False
      False
      False
      False
      False
      Nothing
      Nothing
      False

    -- TODO(skylar): This can just be a shell or proc instead of doing this by hand
    cp =
      CreateProcess
      (ShellCommand $
       intercalate " " [ ckbPath
                       , "init"
                       , "--chain"
                       , "dev"
                       , bool "" "--force" force
                       ])
      (Just path)
      Nothing
      Inherit
      Inherit
      Inherit
      False
      False
      False
      False
      False
      False
      Nothing
      Nothing
      False

runDevelopmentChain :: MonadIO m => FilePath -> m ()
runDevelopmentChain directory = liftIO $ do
  exists <- doesDirectoryExist directory
  case exists of
    False -> pure ()
    True -> removePathForcibly directory
  createDirectoryIfMissing False directory
  -- TODO(skylar): Can we just detect if this should be forced, and avoid creating this everytime?
  initDevChain directory True
  pure ()
