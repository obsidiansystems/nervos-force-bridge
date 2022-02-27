{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CKB.Types where

import System.Which
import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Aeson.Types as Aeson

import Control.Applicative ((<|>))

import Data.Attoparsec.Text as A

import GHC.Generics

ckbPath :: FilePath
ckbPath = $(staticWhich "ckb")

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

capsulePath :: FilePath
capsulePath = $(staticWhich "capsule")

newtype Address =
  Address { unAddress :: T.Text }
  deriving (Eq, Show)

newtype TestnetAddress =
  TestnetAddress { unTestnetAddress :: T.Text }
  deriving (Eq, Show)

deriveJSON defaultOptions ''Address
deriveJSON defaultOptions ''TestnetAddress

data Account =
  Account { mainnet_address :: Address
          , testnet_address :: TestnetAddress
          -- TODO(skylar): Typing for this?
          , lock_arg :: T.Text
          -- NOTE(skylar): Is Just if it was generated
          , lock_hash :: Maybe T.Text
          }
  deriving (Eq, Show, Generic)

-- TODO(skylar): This whole instance can be done less stupid
instance FromJSON Account where
  parseJSON = withObject "Account" $ \v ->
    Account <$> v .: "mainnet"
            <*> v .: "testnet"
            <*> v .: "lock_arg"
            <*> v .: "lock_hash"

instance ToJSON Account where
  toJSON (Account m t la lh) =
    object [ "mainnet" .= m
           , "testnet" .= t
           , "lock_arg" .= la
           , "lock_hash" .= lh
           ]

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

account :: Parser Account
account = do
  Account
    <$> (Address <$> mainnet)
    <*> (TestnetAddress <$> testnet)
    <*> addr "lock_arg"
    -- TODO(skylar): There must be a better way to express this
    <*> ((Just <$> addr "lock_hash") <|> pure Nothing)
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
