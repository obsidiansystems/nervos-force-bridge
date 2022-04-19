{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-} 

{- |
Description: Common bridge types and configuration that are shared between chains 
-}
module Common.Bridge where

import Data.Text as T
import Data.Aeson
import Data.Aeson.TH



newtype CKBAddress =
  CKBAddress { unCKBAddress :: T.Text }
  deriving (Eq, Show)


newtype AdaTxHash =
  AdaTxHash { unAdaTxHash :: T.Text }
  deriving (Eq, Show, Ord)


instance FromJSON AdaTxHash where
  parseJSON = withObject "TxHash" $ \o ->
    AdaTxHash <$> o .: "tx_hash"

instance ToJSON AdaTxHash where
  toJSON (AdaTxHash h) = object [ "tx_hash" .= h
                                ]

deriveJSON defaultOptions ''CKBAddress
