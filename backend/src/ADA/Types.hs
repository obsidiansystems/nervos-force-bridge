{-# LANGUAGE TemplateHaskell #-}

module ADA.Types where

import System.Which
-- import qualified Data.Text as T

cardanoPath :: FilePath
cardanoPath = $(staticWhich "cardano-node")
