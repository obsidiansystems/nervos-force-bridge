{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Bridge.Nervos.Types where

import Data.Word
import qualified Data.Text as T

import GHC.Generics

import Data.Aeson
import Data.Aeson.TH

import Bridge.Utils


import Data.Attoparsec.Text as A
-- import GHC.Generics

newtype CKBytes =
  CKBytes { unCKBytes :: Integer }
  deriving (Eq, Show, Ord)

-- TODO Naming for everything that is json compatible

-- TODO what about testnet vs mainnet
newtype Address =
  Address { unAddress :: T.Text }
  deriving (Eq, Show)

instance FromJSON Address where
  parseJSON =
    withText "Address" (\t -> pure $ Address t)

instance ToJSON Address where
  toJSON (Address t) = String t

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
-- deriveJSON (scrubPrefix "liveCell_") ''LiveCell
deriveJSON (scrubPrefix "liveCells_") ''LiveCells

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
