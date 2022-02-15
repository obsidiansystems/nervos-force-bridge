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

import Prettyprinter (pretty)

import Data.Text as T

import CKB

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- runDevelopmentChain "ckb"
      -- ckbInitDev
      -- flip runLoggingT (print . renderWithSeverity id) $ do
      -- logMessage $ WithSeverity Informational (pretty gitPath)
      serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
