{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module CKB.Capsule where

import CKB.Types
import System.Process
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log
import qualified Data.Text as T

import Backend.Utils

-- TODO(skylar): Can we statically do stuff related to capsule projects?
-- TODO(sklyar): Potentially
-- TODO(skylar): Isomorphic bool for releasev vs debug?
-- TODO(skylar): This doesn't work lmao
buildProject :: ForceM m => FilePath -> m ()
buildProject cfp = do
  _ <- liftIO $ createProcess $ inDirectory cfp $ proc capsulePath [ "build"
                                                                   , "--release"
                                                                   ]
  pure ()
-- 6234600010000
-- 20000000000
-- TODO(skylar): This is only on testnet
-- TODO(skylar): Monitor deployment toml
-- TODO(skylar): Monitor the migrations directory post deployment
deployProject :: ForceM m => Account -> FilePath -> m ()
deployProject account cfp = do
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
                                                  $ account
                                                ]
      {-procWithCkbCliIn "./ckb/.ckb-cli" cfp capsulePath [ "deploy"
                                                      , "--address"
                                                      , T.unpack
                                                        $ unTestnetAddress
                                                        $ testnet_address
                                                        $ account
                                                      ]-}
    {-mkDeployProc = do
      procWithCkbCliIn "./ckb/.ckb-cli" cfp capsulePath [ "deploy"
                                                      , "--address"
                                                      , T.unpack
                                                        $ unTestnetAddress
                                                        $ testnet_address
                                                        $ account
                                                      ]-}
