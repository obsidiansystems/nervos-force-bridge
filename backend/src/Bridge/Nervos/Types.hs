{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Bridge.Nervos.Types where

import Data.Word
import qualified Data.Text as T

import GHC.Generics

import Data.Aeson
import Data.Aeson.TH

import Bridge.Utils

import Data.Attoparsec.Text as A

data MintTx =
  MintTx { txHash :: TxHash
         , mintTo :: Script
         , mintAmount :: Integer
         }
  deriving (Eq, Show)

newtype CKBytes =
  CKBytes { unCKBytes :: Integer }
  deriving (Eq, Show, Ord)

data HashType =
  HashTypeType | HashTypeData
  deriving (Eq, Show)

data Script = Script
  { script_code_hash :: T.Text
  , script_hash_type :: HashType
  , script_args :: T.Text
  }
  deriving (Eq, Show)

newtype Address =
  Address { unAddress :: T.Text }
  deriving (Eq, Show)

instance FromJSON Address where
  parseJSON =
    withText "Address" (\t -> pure $ Address t)

instance ToJSON Address where
  toJSON (Address t) = String t

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

data LiveCell = LiveCell
  { liveCell_capacity :: CKBytes
  , liveCell_tx_hash :: T.Text
  , liveCell_output_index :: Int
  }
  deriving (Eq, Show, Generic)

data GetCellsResult = GetCellsResult
  { getCellsResult_objects :: [LiveCell]
  }
  deriving (Eq, Show)

data LiveCells = LiveCells
  { liveCells_total_capacity :: CKBytes
  , liveCells_total_count :: Int
  , liveCells_live_cells :: [LiveCell]
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

diffCkb :: CKBytes -> CKBytes -> CKBytes
diffCkb (CKBytes f) (CKBytes s) = CKBytes $ f - s

shannons :: Integer -> CKBytes
shannons = CKBytes

ckb :: Double -> CKBytes
ckb n = CKBytes $ truncate $ n * ckbInShannons

ckbytesToDouble :: CKBytes -> Double
ckbytesToDouble (CKBytes s) = fromIntegral (s :: Integer) / fromIntegral (ckbInShannons :: Integer)

deployedSUDT :: Script
deployedSUDT = Script
  "0xc5e5dcf215925f7ef4dfaf5f4b4f105bc321c02776d6e7d52a1db3fcd9d011a4"
  HashTypeType
  "0x15cec0cbd70ba5a93d6e0620893cfe6159c9b3c7ce25dc8541f567fc19f03855"

deployedSUDTDep :: CellDep
deployedSUDTDep = CellDep
  (OutPoint "0xe12877ebd2c3c364dc46c5c992bcfaf4fee33fa13eebdf82c591fc9825aab769" "0x0")
  Code

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

instance FromJSON LiveCell where
  parseJSON = withObject "LiveCell" $ \o -> do
    output <- o .: "output"
    outPoint <- o .: "out_point"
    -- NOTE we are parsing out a word64 from the 5 byte bytestring
    capStr <- output .: "capacity"
    let
      withoutPrefix = T.drop 2 capStr
      padding = T.take (16 - T.length withoutPrefix) "0000000000000000"
      mCap = fromHexUtf8 . (padding <>) $ withoutPrefix

    case mCap of
      Nothing -> fail "Not a valid capacity"
      Just (capacity :: Word64) -> do
        hash <- outPoint .: "tx_hash"
        -- IMPORTANT NOTE this will fail on bigger indices
        mIndex :: Maybe Word8 <- fromHexUtf8 . ("0" <>) . T.drop 2 <$>  outPoint .: "index"
        case mIndex of
          Nothing -> fail "Not a valid index"
          Just index ->
            pure $ LiveCell (CKBytes $ toInteger capacity) hash (fromIntegral index)

-- NOTE(skylar): This instance is not compatible with the FromJSON instance
instance ToJSON LiveCell

deriveJSON (scrubPrefix "getCellsResult_") ''GetCellsResult
deriveJSON (scrubPrefix "script_") ''Script
deriveJSON (scrubPrefix "outPoint_") ''OutPoint
deriveJSON (scrubPrefix "liveCells_") ''LiveCells
deriveJSON (scrubPrefix "cellDep_") ''CellDep

instance FromJSON CKBytes where
  parseJSON = withText "CKBytes" $ \t -> do
    case parseOnly ckbytes t of
      Right b -> pure b
      Left _ ->
        fail "Parsing CKBytes failed"

instance ToJSON CKBytes where
  toJSON (CKBytes s) = String $ (T.pack . show
                                 $ (fromIntegral (s :: Integer) / fromIntegral (ckbInShannons :: Integer) :: Double))
                       <> " (CKB)"
