{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} 

module Nervos where

import Language.Javascript.JSaddle ( MonadJSM, liftJSM)
import Reflex.Dom.Core hiding (Value)
import Common.Bridge 
import Nami 

import Control.Monad ((<=<))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Applicative((<|>))
import Data.ByteString (ByteString)

import Data.Maybe
import qualified Data.Text as T
import Data.Aeson
import qualified Network.JsonRpc.TinyClient as JRPC -- is just the JSON

import Common.Nervos as CKB

-- | I honestly just copied these from
-- | the JSONRpc library 
-- | JSON-RPC request.
data RpcRequest = RpcRequest
    { rqMethod :: !JRPC.MethodName
    , rqId     :: !Int  -- TODO(galen): make this build a random num like the lib
    , rqParams :: !Value
    }
    deriving (Eq, Show)

instance ToJSON RpcRequest where
    toJSON rq = object [ "jsonrpc" .= String "2.0"
                       , "method"  .= rqMethod rq
                       , "params"  .= rqParams rq
                       , "id"      .= rqId rq -- TODO(galen): make this build a random num like the lib
                       ]

-- | Main motive here is to use the JSON instance for RPC that is already defined
mkRPCRequest :: JRPC.MethodName -> [Value] -> RpcRequest
mkRPCRequest method params =
  let rpcId = 2
  in RpcRequest method rpcId (toJSON params) 

-- :: ByteString }} IsXhrPayload 
-- encode . mkRPCRequest "method" [(toJSON searchKey), (toJSON Desc), (toJSON "0x64")]


-- | history -> foundTxs ==> append

-- Map AdaTxHash 

filterUsersMints :: [TxHash] -> [MintTx] -> [(MintTx, AdaTxHash)]
filterUsersMints = undefined


-- updateBridgeInTxs :: Map TxHash BridgeInTx -> [MintTx] -> Map TxHash BridgeInTx
-- updateBridgeInTxs mappy minty =
--   let
--     ks = keys mappy
--     re

-- for each entry, check if txhash is the same as the assocLockTx and if so then change the BridgeInTx 


-- f :: [(TxHash, BridgeInTx)] -> [MintTx] -> [(TxHash, BridgeInTx)]
-- f [] minty = [] 
-- f (x:xs) minty = tryUpdate x minty : f xs minty
--   where
--     tryUpdate :: (TxHash, BridgeInTx) -> [MintTx] -> (TxHash, BridgeInTx)
--     tryUpdate tx [] = tx 
--     tryUpdate (txh, btx) (mint:mints) = if ((unAdaTxHash . assocLockTxHash $ mint) == txh)
--                                         then undefined -- (txh, btx { ckbTxHash = Just $ mint } )
--                                         else "" 
                                                       

  
getAllMintTxs :: ( MonadJSM (Performable m)
                 , MonadJSM m
                 , Monad m
                 , PerformEvent t m
                 , TriggerEvent t m
                 , DomBuilder t m
                 , PostBuild t m
                 ) =>
                 Event t () 
              -> m (Event t [MintTx]) 
getAllMintTxs click = do
  emSearchResults <- getTransactions click
  liftJSM $ clog $ T.pack "got here"
  let
--    eTxRecords :: Event t [TxRecord]
    eTxRecords = fmap (fromMaybe [] . (fmap searchResults_objects)) emSearchResults

    getMintTxReqs = (fmap.fmap) mkReq eTxRecords 
  (responses :: Event t [XhrResponse]) <- performRequestsAsync getMintTxReqs 
  let
    script = CKB.deployedSUDT

    -- gets all MintTxs from the CkbTxInfo
    mintTxs :: Event t [MintTx]
    mintTxs =  fmap mconcat $ (fmap.fmap) ((getMints script) . txInfo_transaction)
               $ fmap catMaybes $ (fmap . fmap) decodeJRPC responses

  pure mintTxs
    where
      mkReq :: TxRecord -> XhrRequest T.Text
      mkReq (TxRecord hash) = postJson "http://obsidian.webhop.org:9114" $ mkRPCRequest "get_transaction" [(toJSON hash)]

 
 

-- getTransactionXHR :: ( MonadJSM (Performable m)
--                      , Monad m
--                      , PerformEvent t m
--                      , TriggerEvent t m 
--                      ) =>
--                      Event t () -> TxRecord -> m (Event t (Maybe CkbTxInfo))
-- getTransactionXHR e (TxRecord hash) = do
--   let
--     -- this is just cargo-culted from getTransaction in the backend
--     req = postJson "http://obsidian.webhop.org:9114"
--           $ mkRPCRequest "get_transaction" [(toJSON hash)]

--   -- this may actually even be plural in event land
--   response <- performRequestAsync (req <$ e)
--   pure $ decodeJRPC <$> response 


-- | This hardcodes our lockscript in the search key
getTransactions :: ( MonadJSM (Performable m)
                   , MonadJSM m
                   , Monad m
                   , PerformEvent t m
                   , TriggerEvent t m
                   , PostBuild t m 
                   ) =>
                   Event t a -- this will probably be postbuild or something
                -> m (Event t (Maybe SearchResults))
getTransactions e = do
  let
    searchKey = SearchKey deployedSUDT Type 
    req2 :: XhrRequest T.Text
    req2 = postJson "http://obsidian.webhop.org:9116"
           $ mkRPCRequest "get_transactions" [(toJSON searchKey), (toJSON Desc), (toJSON ("0x64" :: T.Text))]
    req' = req2 <$ e
  liftJSM $ clog $ T.pack "doing get transactions"
  liftJSM $ clog $ T.pack . show $ req2
  (response :: Event t XhrResponse) <- performRequestAsync req'
--  liftJSM $ clog $ T.pack . show $ 
  pure $ decodeJRPC <$> response 
 
decodeJRPC :: FromJSON a => XhrResponse -> Maybe a
decodeJRPC x = result <$> decodeXhrResponse x 


-- TODO(galen): is there a way to get rid of this wrapper
data JResponse a = JResponse { result :: a } deriving (Eq, Show)

instance FromJSON a => FromJSON (JResponse a) where
  parseJSON =
    withObject "JSON-RPC response object" $
    \v -> JResponse <$> v .: "result"
--          (Right <$> v .: "result" <|> Left <$> v .: "error")
