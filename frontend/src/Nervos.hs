{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-} 

module Nervos where

import Language.Javascript.JSaddle ( MonadJSM )
import Reflex.Dom.Core hiding (Value)
import Common.Nervos

import Data.ByteString (ByteString)

import Data.Maybe
import qualified Data.Text as T
import Data.Aeson
import qualified Network.JsonRpc.TinyClient as JRPC -- is just the JSON

import Bridge.Nervos as CKB 
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

getAllMintTxs :: ( MonadJSM (Performable m)
                 , Monad m
                 , PerformEvent t m
                 , TriggerEvent t m
                 , DomBuilder t m
                 ) =>
                 m (Event t [MintTx]) 
getAllMintTxs = do
  click <- button "click me"
  emSearchResults <- getTransactions click 
  let
--    eTxRecords :: Event t [TxRecord]
    eTxRecords = fmap (fromMaybe [] . (fmap searchResults_objects)) emSearchResults

    getMintTxReqs = (fmap.fmap) mkReq eTxRecords 
  (responses :: Event t [XhrResponse]) <- performRequestsAsync getMintTxReqs 
  let
    script = CKB.deployedSUDT
    
    mintTxs :: Event t [MintTx]
    mintTxs =  fmap mconcat $ (fmap.fmap) ((getMints script) . txInfo_transaction)
               $ fmap catMaybes $ (fmap . fmap) decodeXhrResponse responses

  pure mintTxs
  where   
    mkReq :: TxRecord -> XhrRequest T.Text
    mkReq (TxRecord hash) = postJson "http://obsidian.webhop.org:9116" $ mkRPCRequest "get_transaction" [(toJSON hash)]



-- | This hardcodes our lockscript in the search key
getTransactions :: ( MonadJSM (Performable m)
                   , Monad m
                   , PerformEvent t m
                   , TriggerEvent t m 
                   ) =>
                   Event t a -- this will probably be postbuild or something
                -> m (Event t (Maybe SearchResults))
getTransactions e = do
  let
    searchKey = SearchKey deployedSUDT Type 
    req2 :: XhrRequest T.Text
    req2 = postJson "http://obsidian.webhop.org:9114"
           $ mkRPCRequest "get_transactions" [(toJSON searchKey), (toJSON Desc), (toJSON ("0x64" :: T.Text))]
    req' = req2 <$ e
  (response :: Event t XhrResponse) <- performRequestAsync req'
  pure $ decodeXhrResponse <$> response 
  
 
-- x = toStrict $ encode $ mkRPCRequest "get_transactions" [(toJSON searchKey), (toJSON Desc), (toJSON "0x64")]
--   where
--     searchKey = SearchKey script _ Type 
    
-- -- | PSEUDO as of now to template getTransaction
-- -- | still need to figure out the reflex flow 
-- f :: Maybe SearchResults -> _ 
-- f = case response of
--       Just searchResults -> do
--         let
--           txHashes = fmap txRecord_tx_hash $ searchResults_objects searchResults
--         for txHashes $ \hash -> do
--           tx <- getTransaction hash
--           pure tx
--       Nothing -> "No transactions" 
  


-- I think that 
getTransactionXHR :: ( MonadJSM (Performable m)
                     , Monad m
                     , PerformEvent t m
                     , TriggerEvent t m 
                     ) =>
                     Event t () -> TxRecord -> m (Event t (Maybe CkbTxInfo))
getTransactionXHR e (TxRecord hash) = do
  let
    -- this is just cargo-culted from getTransaction in the backend
    req = postJson "http://obsidian.webhop.org:9116"
          $ mkRPCRequest "get_transaction" [(toJSON hash)]

  -- this may actually even be plural in event land
  response <- performRequestAsync (req <$ e)
  pure $ decodeXhrResponse <$> response 





                    
