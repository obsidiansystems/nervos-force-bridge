{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CKB.Types where

import System.Which
import qualified Data.Text as T

import Data.Aeson
import qualified Data.Aeson.Types as Aeson

import Data.Attoparsec.Text as A

ckbPath :: FilePath
ckbPath = $(staticWhich "ckb")

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

capsulePath :: FilePath
capsulePath = $(staticWhich "capsule")

data Account =
  Account { mainnet_address :: Address
          , testnet_address :: TestnetAddress
          -- TODO(skylar): Typing for this?
          , lock_arg :: T.Text
          , lock_hash :: T.Text
          }
  deriving (Eq, Show)

-- TODO(skylar): Does this make sense
data Capacity = Capacity
   { total :: CKBytes
   }
  deriving (Eq, Show)

instance FromJSON Capacity where
  parseJSON = withObject "Capacity" $ \v -> do
    Capacity <$> v .: "total"

-- NOTE(skylar): Stored in Shannons
-- TODO(skylar): Do we wanna store this in Shannons?
newtype CKBytes =
  CKBytes { unCKBytes :: Int }
  deriving (Eq, Show, Num, Ord)

instance FromJSON CKBytes where
  parseJSON = withText "CKBytes" $ \t -> do
    case parseOnly ckbytes t of
      Right b -> pure b
      Left err ->
        fail "Parsing CKBytes failed"


newtype Address =
  Address { unAddress :: T.Text }
  deriving (Eq, Show)

newtype TestnetAddress =
  TestnetAddress { unTestnetAddress :: T.Text }
  deriving (Eq, Show)

account :: Parser Account
account = do
  Account
    <$> (Address <$> mainnet)
    <*> (TestnetAddress <$> testnet)
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

ckbytes :: Parser CKBytes
ckbytes = do
  n <- double
  skipMany space
  string "(CKB)"
  pure $ CKBytes $ truncate $ n * 100000000
  where
