{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: CKB Chain configuration types
-}

module CKB.Config where

import CKB.Types

import GHC.Generics
import qualified Data.Text as T

import Toml.Codec ( TomlCodec
                  , HasCodec
                  , genericCodec
                  )
import qualified Toml.Codec as Toml

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
  Logger { filter :: T.Text
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
  "0x"
  where
    magicHash = "0x9bd7e06f3ecf4be0f2fcd2188b23f1b9fcc88e5d4b65a8637b17723bbda3cce8"
    alwaysHashType = "type"

ckbConfigCodec :: TomlCodec CkbConfig
ckbConfigCodec = genericCodec

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
