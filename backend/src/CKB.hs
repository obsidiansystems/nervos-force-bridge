{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CKB (runDevelopmentChain) where

import Control.Monad.IO.Class

import System.Which
import System.Process
import System.Directory

import Data.Bool
import Data.List

ckbPath :: FilePath
ckbPath = $(staticWhich "ckb")

-- TODO(skylar): This fails if certain files already exist
initDevChain :: MonadIO m => FilePath -> Bool -> m ()
initDevChain path force = liftIO $ do
  _ <- createProcess cp
  pure ()
  where
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
