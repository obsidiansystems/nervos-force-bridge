{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 

module Bridge.Cardano where

import qualified Data.Text as T

import Data.Maybe
import Data.Traversable

-- import Data.Map (Map)
import qualified Data.Map as Map

import Bridge.Nervos.Cli (getAddressInfo)

import Bridge.Utils
import Bridge.Cardano.Types (Address, LockTx(..), AssetType(..))
import qualified Bridge.Cardano.Blockfrost as BF
-- TODO Unify these
import qualified Bridge.Nervos.Types as CKB

import Common.Cardano (CardanoBridgeMetadata(..))
import Common.Bridge (CKBAddress(..))

getLockTxsAt :: BridgeM m => BF.ApiKey -> Address -> m [LockTx]
getLockTxsAt key addr = do
  -- logDebug "Getting lock txns"
  txHashes <- BF.getTransactions key addr
  -- logDebug "Got here"
  mLocks <- for txHashes $ \hash -> do
    mMeta :: Maybe CardanoBridgeMetadata <- BF.getTransactionMetadata key hash
    mScript <- case mMeta of
      Nothing -> pure Nothing
      Just md -> do
        -- logDebug "Found lock metadata"
        fmap Just $ getAddressInfo $ CKB.Address $ unCKBAddress $ mintToAddress md
    paid <- BF.getValuePaidTo key addr hash
    let lovelace = Map.findWithDefault 0 Ada paid
    pure $ (\scr -> LockTx hash scr lovelace) <$> mScript

  let locks = catMaybes mLocks
  logDebug $ "Found " <> (T.pack . show $ length locks) <> " lock txns"
  pure locks
