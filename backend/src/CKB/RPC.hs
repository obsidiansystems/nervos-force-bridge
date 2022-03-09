{-# LANGUAGE OverloadedStrings #-}

{-|
Description: CKB RPC calls and Providers
-}

module CKB.RPC where

import qualified Data.Text as T

import Network.Web3.Provider
import Network.JsonRpc.TinyClient

localhostEndpoint :: String
localhostEndpoint = "http://127.0.0.1:8114"

defaultLocalhostProvider :: Provider
defaultLocalhostProvider =
  HttpProvider localhostEndpoint

getTipBlockNumber :: JsonRpc m => m T.Text
getTipBlockNumber = remote "get_tip_block_number"
