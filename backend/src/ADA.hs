{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module ADA (runDevelopmentChain) where

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

