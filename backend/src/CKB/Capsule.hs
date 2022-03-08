{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Description: CKB Capsule deployment utilities
-}

module CKB.Capsule where

import CKB.Types
import System.Process
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log
import qualified Data.Text as T

import Backend.Utils

deployProject :: ForceM m => Account -> FilePath -> m ()
deployProject acc cfp = do
  logInfo "Starting deployment"
  capsuleDeployProc <- mkDeployProc'
  output <- liftIO $ readCreateProcess capsuleDeployProc "y\nhello\nhello"
  logInfo $ T.pack output
  pure ()
  where
    mkDeployProc' = do
      pure $ inDirectory cfp $ proc capsulePath [ "deploy"
                                                , "--address"
                                                , T.unpack
                                                  $ unTestnetAddress
                                                  $ testnet_address
                                                  $ acc
                                                ]
