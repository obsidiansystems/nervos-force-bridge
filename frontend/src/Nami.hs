{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nami where

import JSDOM
import JSDOM.Types (fromJSArrayUnchecked)

import Control.Lens
import Control.Monad.IO.Class

import Promise

import Data.Time

import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.TH
import Data.HexString
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as LBS

import Language.Javascript.JSaddle ( eval
                                   , liftJSM
                                   , ghcjsPure
                                   , MonadJSM

                                   , ToJSVal

                                   , fromJSValUnchecked
                                   , fromJSVal
                                   , isNull

                                   , js
                                   , jsg
                                   , js0
                                   , js1
                                   , js2
                                   , js3
                                   , js4

                                   , fun

                                   , MakeObject
                                   , JSVal
                                   , JSM
                                   )

import Codec.Serialise
import GHC.Natural

data APIError = APIError String
type Address = T.Text


data Status
  = LockSubmitted
  | LockAwaitingConfirmations Int
  | AwaitingMint
  | Bridged UTCTime
  deriving (Eq, Show)

type TxHash = T.Text
newtype CKBAddress =
  CKBAddress { unCKBAddress :: T.Text }
  deriving (Eq, Show)

mkCKBAddress :: T.Text -> Maybe CKBAddress
mkCKBAddress t
  | T.length t == targetLen && T.isPrefixOf (T.pack "ckt") t = Just $ CKBAddress t
  | otherwise = Nothing
  where
    targetLen = T.length $ T.pack "ckt1qyq9u9h0egwgy73qjkz29gzkqq67fdl60r3s2egrpu"

data BridgeInTx =
  BridgeInTx { bridgeInAmount :: Double
             , bridgeInToAddress :: CKBAddress
             , bridgeInTxHash :: TxHash
             , bridgeInTxStatus :: Status
             , bridgeInStart :: UTCTime
             }

deriveJSON defaultOptions ''CKBAddress
deriveJSON defaultOptions ''Status
deriveJSON defaultOptions ''BridgeInTx

newtype NamiApi =
  NamiApi JSVal
  deriving (MakeObject)

-- TODO(skylar): Currently we don't have overloaded strings so "cardano" is a string
-- but eventually we might need overloaded strings...

getApi :: (MonadJSM m) => m (Either APIError NamiApi)
getApi = liftJSM $ do
  window <- currentWindowUnchecked
  promise <- window ^. js "cardano" . js "nami" . js0 "enable"
  handlePromise (unsafeToPromise promise) (pure . NamiApi) (const $ pure $ APIError "Nami Wallet Not Enabled")

getBalance :: MonadJSM m => NamiApi -> m Double
getBalance napi = liftJSM $ do
  promise <- napi ^. js0 "getBalance"
  x <- promiseMaybe (unsafeToPromise promise) (fromJSValUnchecked)
  case x of
    Nothing -> pure 0
    Just (j :: T.Text) -> pure $ fromIntegral val / 1000000
      where
        val :: Natural
        val = deserialise $ LBS.fromStrict $ toBytes $ hexString $ encodeUtf8 j

getUsedAddress :: (MonadJSM m) => NamiApi -> m (Maybe Address)
getUsedAddress napi = liftJSM $ do
  promise <- napi ^. js0 "getUsedAddresses"
  -- TODO(skylar): This is really a MaybeT
  mthing <- promiseMaybe (unsafeToPromise promise) (pure . id)
  case mthing of
    Nothing -> pure Nothing
    Just v -> convert v
  where
    -- TODO(skylar): This is pretty much [a] -> Maybe a
    convert val = do
      results :: [JSVal] <- fromJSArrayUnchecked val
      case results of
        [addr] -> Just <$> addressFromBytes addr
        _ -> pure Nothing

-- TODO(skylar): Merge this with getUsedAddress...
getChangeAddress :: (MonadJSM m) => NamiApi -> m (Maybe Address)
getChangeAddress napi = liftJSM $ do
  promise <- napi ^. js0 "getChangeAddress"
  -- TODO(skylar): This is really a MaybeT
  mthing <- promiseMaybe (unsafeToPromise promise) (pure . id)
  case mthing of
    Nothing -> pure Nothing
    Just v -> convert v
  where
    -- TODO(skylar): This is pretty much [a] -> Maybe a
    convert val = do
      Just <$> addressFromBytes val

clog :: ToJSVal a => a -> JSM ()
clog a = do
  _ <- jsg "console" ^. js1 "log" a
  pure ()

-- TODO(skylar): Newtype this?
addressFromBytes :: JSVal -> JSM Address
addressFromBytes v = do
  buffer <- jsg "Buffer" ^. js2 "from" v "hex"
  addr <- jsg "CardanoWasm" ^. js "Address" . js1 "from_bytes" buffer
  addr ^. js0 "to_bech32" >>= fromJSValUnchecked

type UTXO = JSVal

type Ada = Double
type Lovelace = Integer
-- TODO(skylar): Where does Value get imported, and why is it higher kinded?
type V = JSVal

-- TODO(skylar): Round or floor (or truncate)?
toLovelace :: Ada -> Lovelace
toLovelace ada = floor $ ada * 1000000

lovelaceToValue :: MonadJSM m => Lovelace -> m V
lovelaceToValue ll = liftJSM $ do
  bn <- cardanoWasm ^. js "BigNum" . js1 "from_str" (show ll)
  val <- cardanoWasm ^. js "Value" . js1 "new" bn
  pure val


data TransactionInput =
  TransactionInput { sender :: Address
                   , outputs :: [(Address, Ada)]
                   }

deepakBech32 :: Address
deepakBech32 =
  T.pack "addr_test1qqvyvv446w768jj42wxrh99mpmk5kd2qppst0yma8qesllldkdcxe8fngwj6m2f9uk5k8unf94tzzryz7kujnnew29xse6rxsu"

{-
TxHash comes from wallet, but may be waiting to hit a mempool
-- TODO Then it hits the mempool of the node
TxWaiting for confirmations (with waiting confirmations)
Locked
-}

data NamiError
  = UserDeclined
  | TxSendFailure
  deriving (Eq, Show)

data BridgeRequest =
  BridgeRequest { bridgeRequestAmount :: Double
                , bridgeRequestTo :: CKBAddress
                }

doPay :: MonadJSM m => NamiApi -> Address -> BridgeRequest -> m (Either NamiError BridgeInTx)
doPay napi from (BridgeRequest amount to) = do
  result <- pay napi from deepakBech32 amount
  utc <- liftIO $ getCurrentTime
  pure $ fmap (\txHash -> BridgeInTx amount to txHash LockSubmitted utc) result

pay :: MonadJSM m => NamiApi -> Address -> Address -> Ada -> m (Either NamiError TxHash)
pay napi from to ada = liftJSM $ do
  tbuild <- newTransactionBuilder
  clog tbuild

  val <- lovelaceToValue lovelace
  utxos <- getUtxos napi (Just val)
  mapM_ (addInput tbuild) utxos

  output <- newTransactionOutput to ada
  tbuild ^. js1 "add_output" output
  -- TODO(skylar): We need to calculate this..
  tbuild ^. js1 "set_ttl" (52463794 :: Int)

  changeAddress <- getChangeAddress napi
  case changeAddress of
    Nothing -> error ":("
    Just addr -> do
      notBech <- cardanoWasm ^. js "Address" . js1 "from_bech32" addr
      tbuild ^. js1 "add_change_if_needed" notBech

  emptyWitnesses <- cardanoWasm ^. js "TransactionWitnessSet" . js0 "new"
  tbody <- tbuild ^. js0 "build"
  unsignedTx <- transactionCbor tbody emptyWitnesses
  witnessesResult <- signTx napi unsignedTx
  case witnessesResult of
     Left err -> pure $ Left err
     Right witnesses -> do
       signedTx <- transactionCbor tbody witnesses

       clog signedTx

       hash <- submitTx napi signedTx
       case hash of
         Nothing -> do
           pure $ Left TxSendFailure
         Just h ->
           Right <$> fromJSValUnchecked h
  where
    lovelace = toLovelace ada

transactionCbor :: MonadJSM m => TxBody -> TransactionWitnessSet -> m Transaction
transactionCbor tbody witnesses  = liftJSM $ do
  tx <- cardanoWasm ^. js "Transaction" . js2 "new" tbody witnesses
  txBytes <- tx ^. js0 "to_bytes"
  buff <- jsg "Buffer" ^. js2 "from" txBytes "hex"
  buff ^. js1 "toString" "hex"

transactionFromBytes :: MonadJSM m => JSVal -> m TransactionWitnessSet
transactionFromBytes v = liftJSM $ do
  buffer <- jsg "Buffer" ^. js2 "from" v "hex"
  jsg "CardanoWasm" ^. js "TransactionWitnessSet" . js1 "from_bytes" buffer

-- IMPORTANT TODO(skylar): This is like cbor hex or someshit
type TxBody = JSVal
type Transaction = JSVal
type TransactionWitnessSet = JSVal
type TransactionHash = JSVal

signTx :: MonadJSM m => NamiApi -> Transaction -> m (Either NamiError TransactionWitnessSet)
signTx napi txn = liftJSM $ do
  emptyWitnesses <- cardanoWasm ^. js "TransactionWitnessSet" . js0 "new"
  promise <- napi ^. js1 "signTx" txn
  handlePromise (unsafeToPromise promise) transactionFromBytes (const $ pure $ UserDeclined)
  -- Right . maybe emptyWitnesses id <$> promiseMaybe (unsafeToPromise promise) transactionFromBytes

submitTx :: MonadJSM m => NamiApi -> Transaction -> m (Maybe TransactionHash)
submitTx napi txn = liftJSM $ do
  promise <- napi ^. js1 "submitTx" txn
  promiseMaybe (unsafeToPromise promise) (pure . id)

addInput :: MonadJSM m => TransactionBuilder -> UTXO -> m ()
addInput tb utxo = liftJSM $ do
  address <- utxo ^. js0 "output" . js0 "address"
  input <- utxo ^. js0 "input"
  amount <- utxo ^. js0 "output" . js0 "amount"

  _ <- tb ^. js3 "add_input" address input amount
  pure ()

type BigNum = JSVal
type TransactionBuilder = JSVal

newBigNum :: MonadJSM m => String -> m BigNum
newBigNum s = liftJSM $ cardanoWasm ^. js "BigNum" . js1 "from_str" s

newTransactionBuilder :: MonadJSM m => m TransactionBuilder
newTransactionBuilder = liftJSM $ do
  minA <- newBigNum "44"
  minB <- newBigNum "155381"
  linearFee <- cardanoWasm ^. js "LinearFee" . js2 "new" minA minB
  poolDeposit <- newBigNum "500000000"
  keyDeposit <- newBigNum "2000000"
  coinsPerUtxoWord <- newBigNum "34482"

  let
    maxValueSize = 5000 :: Int
    maxTxSize = 16384 :: Int

    andThen n v x = x ^. js1 n v

  cfgBuilder <-
    cardanoWasm ^. js "TransactionBuilderConfigBuilder" . js0 "new"
    >>= andThen "fee_algo" linearFee
    >>= andThen "pool_deposit" poolDeposit
    >>= andThen "key_deposit" keyDeposit
    >>= andThen "max_value_size" maxValueSize
    >>= andThen "max_tx_size" maxTxSize
    >>= andThen "max_tx_size" maxTxSize
    >>= andThen "coins_per_utxo_word" coinsPerUtxoWord

  cfg <- cfgBuilder ^. js0 "build"
  clog cfg
  cardanoWasm ^. js "TransactionBuilder" . js1 "new" cfg

-- TODO(skylar): Do we even want this format here?
-- TODO(skylar): What is this naming?
type TransactionOutput = JSVal

-- TODO(skylar): Types
cardanoWasm :: JSM JSVal
cardanoWasm = jsg "CardanoWasm"

newTransactionOutput :: MonadJSM m => Address -> Ada -> m TransactionOutput
newTransactionOutput bech32 ada = liftJSM $ do

  addr <- cardanoWasm ^. js "Address" . js1 "from_bech32" bech32

  -- TODO(skylar): Can we do this without the show??
  amount <- newBigNum $ show lovelace
  val <- cardanoWasm ^. js "Value" . js1 "new" amount

  cardanoWasm ^. js "TransactionOutput" . js2 "new" addr val

  where
    lovelace = toLovelace ada

getUtxos :: MonadJSM m => NamiApi -> Maybe V -> m [UTXO]
getUtxos napi mv = liftJSM $ do
  promise <- napi ^. js0 "getUtxos"
  list :: Maybe [JSVal] <- promiseMaybe (unsafeToPromise promise) (fromJSValUnchecked)
  results <- mapM toUTXO $ maybe [] id list
  mapM_ (\x -> jsg "console" ^. js1 "log" x) results
  pure results
  where
    toUTXO raw = do
      buffer <- jsg "Buffer" ^. js2 "from" raw "hex"
      jsg "CardanoWasm" ^. js "TransactionUnspentOutput" . js1 "from_bytes" buffer

signTest :: MonadJSM m => NamiApi -> Address -> m ()
signTest napi addr = liftJSM $ do
  _ <- napi ^. js2 "signData" addr cbor
  pure ()
  where
    message = "RPC Not detected: Testing Sign Functionality Instead, if you see this signing is working"
    cbor = toText $ fromBytes $ LBS.toStrict $ serialise $ T.pack message

testWasm :: MonadJSM m => m ()
testWasm = liftJSM $ do
  _ <- eval "console.log(CardanoWasm)"
  pure ()

writeTxs :: MonadJSM m => Map TxHash BridgeInTx -> m ()
writeTxs txns = liftJSM $ do
  storage <- jsg "localStorage"
  _ <- storage ^. js2 "setItem" "txns" (decodeUtf8 $ LBS.toStrict $ Aeson.encode txns)
  pure ()

updateTx :: MonadJSM m => BridgeInTx -> m ()
updateTx bin = do
  txs <- readTxs
  writeTxs $ Map.update (const $ Just bin) (bridgeInTxHash bin) txs

readTxs :: MonadJSM m => m (Map TxHash BridgeInTx)
readTxs = liftJSM $ do
  clog "Starting read"
  storage <- jsg "localStorage"
  clog storage
  rawVal <- storage ^. js1 "getItem" "txns"
  bNull <- ghcjsPure $ isNull rawVal
  case bNull of
    True -> pure mempty
    False -> do
      transactions <- fromJSVal rawVal
      clog transactions
      clog "Done read"
      case Aeson.decode . LBS.fromStrict . encodeUtf8 =<< transactions of
        Just t -> pure t
        Nothing -> pure mempty
