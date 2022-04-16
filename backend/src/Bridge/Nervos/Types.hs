{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Bridge.Nervos.Types where

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.TH

import Bridge.Utils

import Data.Attoparsec.Text as A
-- import GHC.Generics

-- TODO IMPORTANT What about the Tx hash
data MintTx =
  MintTx { mintTo :: Script
         , mintAmount :: Integer
         }
  deriving (Eq, Show)

newtype CKBytes =
  CKBytes { unCKBytes :: Integer }
  deriving (Eq, Show, Ord)

data HashType =
  HashTypeType | HashTypeData
  deriving (Eq, Show)

-- TODO Naming for everything that is json compatible
data Script = Script
  { script_code_hash :: T.Text
  , script_hash_type :: HashType
  , script_args :: T.Text
  }
  deriving (Eq, Show)

-- TODO what about testnet vs mainnet
newtype Address =
  Address { unAddress :: T.Text }
  deriving (Eq, Show)

newtype TxHash =
  TxHash { unTxHash :: T.Text }
  deriving (Eq, Show)

data DeployedScript =
  DeployedScript { deployedScriptScript :: Script
                 , deployedScriptDep :: CellDep
                 }
  deriving (Eq)

data OutPoint = OutPoint
  {
    outPoint_tx_hash :: T.Text
  , outPoint_index :: T.Text
  }
  deriving (Eq, Show)

data DepType =
  Group | Code
  deriving (Eq, Show)

data CellDep = CellDep
  { cellDep_out_point :: OutPoint
  , cellDep_dep_type :: DepType
  }
  deriving (Eq, Show)

ckbytes :: Parser CKBytes
ckbytes = do
  n <- double
  skipMany space
  _ <- string "(CKB)"
  pure $ ckb n

ckbInShannons :: Num a => a
ckbInShannons = 100000000

addCkb :: CKBytes -> CKBytes -> CKBytes
addCkb (CKBytes a) (CKBytes b) = CKBytes $ a + b

-- TODO just use integer
diffCkb :: CKBytes -> CKBytes -> CKBytes
diffCkb (CKBytes f) (CKBytes s) = CKBytes $ f - s

shannons :: Integer -> CKBytes
shannons = CKBytes

-- TODO resuse logic in a single function
ckb :: Double -> CKBytes
ckb n = CKBytes $ truncate $ n * ckbInShannons

ckbytesToDouble :: CKBytes -> Double
ckbytesToDouble (CKBytes s) = fromIntegral s / fromIntegral ckbInShannons

instance ToJSON HashType where
  toJSON dt = String $ case dt of
    HashTypeType -> "type"
    HashTypeData -> "data"

instance FromJSON HashType where
  parseJSON = withText "HashType" $ \case
    "type" -> pure HashTypeType
    "data" -> pure HashTypeData
    _ -> fail "Not a valid HashType"

instance ToJSON DepType where
  toJSON dt = String $ case dt of
    Group -> "dep_group"
    Code -> "code"

instance FromJSON DepType where
  parseJSON = withText "DepType" $ \case
    "dep_group" -> pure Group
    "code" -> pure Code
    _ -> fail "Not a valid DepType"


deriveJSON (scrubPrefix "script_") ''Script

instance FromJSON CKBytes where
  parseJSON = withText "CKBytes" $ \t -> do
    case parseOnly ckbytes t of
      Right b -> pure b
      Left _ ->
        fail "Parsing CKBytes failed"

instance ToJSON CKBytes where
  toJSON (CKBytes s) = String $ (T.pack . show $  fromIntegral s / fromIntegral ckbInShannons) <> " (CKB)"

{-
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
-}
