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

import qualified Toml.Codec as Toml

import GHC.IO.Handle
import Obelisk.Configs
import qualified Obelisk.ExecutableConfig.Lookup as Lookup

import System.IO.Temp
import qualified Data.Text as T

import Data.Attoparsec.Text as A

data CkbConfig =
  CkbConfig { data_dir :: T.Text
            }
  deriving (Generic)

data Account =
  Account { mainnet_address :: Address
          , testnet_address :: Address
          -- TODO(skylar): Typing for this?
          , lock_arg :: T.Text
          , lock_hash :: T.Text
          }
  deriving (Show)

newtype Address =
  Address T.Text
  deriving (Eq, Show)

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

  {-liftIO $ withTempFile "." "password" $ \_ handle -> do
  hPutStr handle "hello"
  hPutStr handle "hello"
  (_, Just stdout, _, _) <- createProcess $ cp handle
  hGetContents stdout >>= putStrLn
  pure undefined
  where
    -- TODO(skylar): Use proc or something here
    cp h =
      CreateProcess
      (ShellCommand $ intercalate " " [ckbCliPath, "account", "new"])
      Nothing
      Nothing
      (UseHandle h)
      Inherit
      Inherit
      False
      False
      False
      False
      False
      False
      Nothing
      Nothing
      False
-}
ckbPath :: FilePath
ckbPath = $(staticWhich "ckb")

ckbCliPath :: FilePath
ckbCliPath = $(staticWhich "ckb-cli")

-- TODO(skylar): This fails if certain files already exist
initDevChain :: (MonadIO m) => FilePath -> Bool -> m ()
initDevChain path force = liftIO $ do
  -- cfgs <- Lookup.getConfigs
  -- Just pass <- runConfigsT cfgs $ getConfig "backend/password"
  -- print pass
  _ <- createProcess cp
  createNewAccount
  pure ()
  where
    -- TODO(skylar): This can just be a shell or proc instead of doing this by hand
    cp =
      CreateProcess
      (ShellCommand $ intercalate " " [ckbPath, "init", "--chain", "dev", bool "" "--force" force])
      (Just path)
      Nothing
      Inherit
      Inherit
      Inherit
      False
      False
      False
      False
      False
      False
      Nothing
      Nothing
      False

runDevelopmentChain :: MonadIO m => FilePath -> m ()
runDevelopmentChain directory = liftIO $ do
  createDirectoryIfMissing False directory
  -- TODO(skylar): Can we just detect if this should be forced, and avoid creating this everytime?
  initDevChain directory True
  pure ()
