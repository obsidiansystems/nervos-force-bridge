{-# LANGUAGE TemplateHaskell #-}

module CKB.Types where

import System.Which
import qualified Data.Text as T

ckbPath :: FilePath
ckbPath = $(staticWhich "ckb")

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

capsulePath :: FilePath
capsulePath = $(staticWhich "capsule")

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
