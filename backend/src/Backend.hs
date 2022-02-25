{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Common.Route
import Obelisk.Backend

import System.Which
import System.Process
import System.Directory

import System.IO (print)
import Control.Monad.Log
import Control.Monad.IO.Class (liftIO, MonadIO)

import Cardano.Binary

import Prettyprinter (pretty)
import Data.Text as T

import CKB
import CKB.RPC

import Control.Concurrent


backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- flip runLoggingT (print . renderWithSeverity pretty) $ runDevelopmentChain "ckb"
      {- forkIO $ do
        -- waitForChain
        threadDelay 1000000
        testRPC -}
      -- ckbInitDev
      -- flip runLoggingT (print . renderWithSeverity id) $ do
        -- logMessage $ WithSeverity Informational "Eheheh"
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
