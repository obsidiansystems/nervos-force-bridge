{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CKB (runDevelopmentChain) where

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

account :: Parser Account
account = do
  Account
    <$> (Address <$> mainnet)
    <*> (Address <$> testnet)
    <*> addr "lock_arg"
    <*> addr "lock_hash"
  where
    addr t = do
      A.takeTill isEndOfLine
      endOfLine
      skipMany space
      string $ t <> ":"
      skipSpace
      takeTill isEndOfLine

    mainnet = addr "mainnet"
    testnet = addr "testnet"

-- We pipe in a temporary file as Stdin to be able to provide a password to the create account utility
createNewAccount :: MonadIO m => m Account
createNewAccount = liftIO $ do
  d <- readProcess ckbCliPath ["account", "new"] input
  let Done _ a = parse account . T.pack $ d
  pure a
  where
    pass = "hello"
    input = intercalate "\n" [pass, pass]

-- TODO(skylar): This fails if certain files already exist
-- Force doesn't actually delete all the things it should :(
-- TODO(skylar): Naming
runDevChain :: (MonadIO m) => FilePath -> Bool -> m ()
runDevChain path force = liftIO $ do
  forceInitChain
  account <- createNewAccount
  r <- Toml.decodeFile ckbConfigCodec "ckb/ckb.toml"
  Toml.encodeToFile ckbConfigCodec "ckb/ckb.toml" $ r { block_assembler = Just $ mkBlockAssembler account }
  runChain
  pure ()
  where
    runChain = createProcess $ inDirectory path $ proc ckbPath ["run"]
    forceInitChain = createProcess $ inDirectory path $ proc ckbPath ["init", "--chain", "dev", bool "" "--force" force]

runDevelopmentChain :: MonadIO m => FilePath -> m ()
runDevelopmentChain directory = liftIO $ do
  exists <- doesDirectoryExist directory
  case exists of
    False -> pure ()
    True -> removePathForcibly directory
  createDirectoryIfMissing False directory
  -- TODO(skylar): Can we just detect if this should be forced, and avoid creating this everytime?
  runDevChain directory True
  pure ()
