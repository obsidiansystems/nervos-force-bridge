{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ADA where

import Control.Monad.IO.Class

import GHC.Generics

import System.Which
import System.Process
import System.Directory

import Data.Bool
import Data.List (intercalate)

import GHC.IO.Handle
import Obelisk.Configs
import qualified Obelisk.ExecutableConfig.Lookup as Lookup

import System.IO.Temp
import qualified Data.Text as T

import Data.Attoparsec.Text as A

import Toml.Codec ( TomlCodec
                  , HasCodec
                  , genericCodec
                  )
import qualified Toml.Codec as Toml

import ADA.Types

import Backend.Utils

-- TODO (alex): include nix-env -iA nixos.cacert on default.nix and 
-- echo "cacert=/etc/ssl/certs/ca-certificates.crt" >> ~/.curlrc
-- for curl.
downloadConfigFiles :: MonadIO m => FilePath -> m ()
downloadConfigFiles path = liftIO $ do
    createProcess $ inDirectory path $ proc "mkdir" ["db"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-topology.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-shelley-genesis.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-config.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-byron-genesis.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-alonzo-genesis.json"]
    return ()
    
runChain :: MonadIO m => FilePath -> m ()
runChain path = liftIO $ do
    createProcess $ inDirectory path $ 
        proc cardanoPath ["run","--topology",topology,"--database-path",db,"--socket-path",socket,"--host-addr",host,"--port",port,"--config",config]
    return ()
    where
        topology = path ++ "/mainnet-topology.json"
        db = path ++ "/db"
        socket = path ++ "/node.socket"
        host = "127.0.0.1"
        port = "3001"
        config = path ++ "/mainnet-config.json"
        

