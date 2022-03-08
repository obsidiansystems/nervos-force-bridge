{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Description: CKB Types and related paths for executables
-}

module CKB.Types where

import System.Which
import System.Directory
import System.Process

import Control.Monad.IO.Class

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.TH

import Control.Applicative ((<|>))

import Data.Attoparsec.Text as A
import GHC.Generics

import Backend.Utils

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
          , lock_arg :: T.Text
          , lock_hash :: Maybe T.Text
          }
  deriving (Eq, Show, Generic)

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

data Capacity = Capacity
   { total :: CKBytes
   }
  deriving (Eq, Show)

instance FromJSON Capacity where
  parseJSON = withObject "Capacity" $ \v -> do
    Capacity <$> v .: "total"

-- NOTE(skylar): Stored in Shannons
newtype CKBytes =
  CKBytes { unCKBytes :: Int }
  deriving (Eq, Show, Ord)

instance FromJSON CKBytes where
  parseJSON = withText "CKBytes" $ \t -> do
    case parseOnly ckbytes t of
      Right b -> pure b
      Left _ ->
        fail "Parsing CKBytes failed"

account :: Parser Account
account = do
  Account
    <$> (Address <$> mainnet)
    <*> (TestnetAddress <$> testnet)
    <*> addr "lock_arg"
    <*> ((Just <$> addr "lock_hash") <|> pure Nothing)
  where
    addr t = do
      _ <- A.takeTill isEndOfLine
      endOfLine
      skipMany space
      _ <- string $ t <> ":"
      skipSpace
      takeTill isEndOfLine

    mainnet = addr "mainnet"
    testnet = addr "testnet"

ckbytes :: Parser CKBytes
ckbytes = do
  n <- double
  skipMany space
  _ <- string "(CKB)"
  pure $ CKBytes $ truncate $ n * 100000000
  where


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
