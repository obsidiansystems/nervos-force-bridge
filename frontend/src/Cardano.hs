{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano where

import Nami
import Language.Javascript.JSaddle
import Reflex.Dom.Core

import Control.Lens

import qualified Data.Text as T

import Data.Aeson
import Data.Aeson.TH

-- data Question
--  = CurrentBlock
--  | TransactionInfo

-- newtype CardanoT t =
-- CardanoT { unCardano :: RequesterT t Question}

-- TODO(skylar): Use better naming, this is just to make the instance easier
data Tx =
  Tx { hash :: T.Text
     -- , block :: T.Text
     , block_height :: Int
     }
  deriving (Eq, Show)

data Block =
  Block { height :: Int
        }
  deriving (Eq, Show)

deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''Block

getTransaction :: ( MonadJSM (Performable m)
                  , PerformEvent t m
                  , TriggerEvent t m
                  ) => Event t TxHash -> m (Event t (Maybe Tx))
getTransaction tx = do
  response <- performRequestAsync $ mkTxRequest <$> tx
  pure $ decodeXhrResponse <$> response
  where
    mkTxRequest h = xhrRequest "GET" ("https://cardano-testnet.blockfrost.io/api/v0/txs/" <> h) $ def
      & xhrRequestConfig_headers .~ "project_id" =: "testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"

getBlock :: ( MonadJSM (Performable m)
            , PerformEvent t m
            , TriggerEvent t m
            ) => Event t () -> m (Event t Block)
getBlock start = do
  response <- performRequestAsync $ blockRequest <$ start
  pure $ maybe (Block 0) id . decodeXhrResponse <$> response
  where
    -- TODO(skylar): Can we leverage getAndDecode with headers?
    blockRequest = xhrRequest "GET" ("https://cardano-testnet.blockfrost.io/api/v0/blocks/latest") $ def
      & xhrRequestConfig_headers .~ "project_id" =: "testnetSZ2mfBUA7l0Ogl5QZu4jGqc0xhg1anq9"
