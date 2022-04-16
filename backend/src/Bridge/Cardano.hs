{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 

module Bridge.Cardano where

import Control.Monad.IO.Class
import qualified Data.Text as T

import Data.Maybe
import Data.Aeson
import Data.Aeson.TH

import Data.Traversable

-- import Data.Map (Map)
import qualified Data.Map as Map

import Bridge.Nervos.Cli (getAddressInfo)

import Bridge.Utils
import Bridge.Cardano.Types as Ada
import qualified Bridge.Cardano.Blockfrost as BF
-- TODO Unify these
import qualified Bridge.Nervos.Types as CKB
import qualified Bridge.Nervos as CKB

getLockTxsAt :: BridgeM m => BF.ApiKey -> Address -> m [LockTx]
getLockTxsAt key addr = do
  txHashes <- BF.getTransactions key addr
  mLocks <- for txHashes $ \hash -> do
    mMeta <- BF.getTransactionMetadata key hash
    mScript <- case mMeta of
      Nothing -> pure Nothing
      Just md -> fmap Just $ getAddressInfo $ CKB.Address $ mintToAddress md
    paid <- BF.getValuePaidTo key addr hash
    let lovelace = Map.findWithDefault 0 Ada paid
    pure $ (\scr -> LockTx hash scr lovelace) <$> mScript
  pure $ catMaybes mLocks
