{-# LANGUAGE OverloadedStrings #-}

module CKB.RPC where

import CKB.Types
import qualified Data.Text as T

import Control.Monad.IO.Class (MonadIO, liftIO)

-- TODO(skylar): This is only importable because web3, and isn't declared in the backend.cabal
import Network.Web3
import Network.Web3.Provider
import Network.JsonRpc.TinyClient

localhostEndpoint :: String
localhostEndpoint = "http://127.0.0.1:8114"

defaultLocalhostProvider :: Provider
defaultLocalhostProvider =
  HttpProvider localhostEndpoint

getTipBlockNumber :: JsonRpc m => m T.Text
getTipBlockNumber = remote "get_tip_block_number"

-- TODO(skylar): Await the dev chain startup

testRPC :: MonadIO m => m ()
testRPC = do
  liftIO $ print "Ayy"
  _ <- runWeb3' defaultLocalhostProvider $ do
    bn <- getTipBlockNumber
    liftIO $ print bn
    pure ()
  pure ()
