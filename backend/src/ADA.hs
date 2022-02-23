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

import CKB.Types
import CKB.Config
import CKB.Capsule

import Backend.Utils

downloadConfigFiles :: MonadIO m => FilePath -> m ()
downloadConfigFiles path = liftIO $ do
    createProcess $ inDirectory path $ proc "mkdir" [path] 
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-topology.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-shelley-genesis.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-config.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-byron-genesis.json"]
    createProcess $ inDirectory path $ proc "curl" ["-O","-J","https://hydra.iohk.io/build/7654130/download/1/testnet-alonzo-genesis.json"]
    return ()
    


