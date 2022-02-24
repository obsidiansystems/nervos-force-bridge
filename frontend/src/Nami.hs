{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nami where

import JSDOM
import JSDOM.Types (liftDOM, fromJSArrayUnchecked)

import Reflex.Dom.Core

import Control.Lens
import Control.Monad.Fix
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Obelisk.Generated.Static

import Promise

import Data.HexString
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.JSString.Text (textFromJSVal)
import Language.Javascript.JSaddle ( eval
                                   , liftJSM
                                   , ghcjsPure
                                   , MonadJSM

                                   , fromJSValUnchecked

                                   , js
                                   , jsg
                                   , js0
                                   , js1
                                   , js2

                                   , fun

                                   , MakeObject
                                   , JSVal
                                   , JSM
                                   )


-- import Codec.CBOR.Decoding
import Codec.Serialise
import GHC.Natural

data APIError = APIError String
type Address = T.Text

newtype NamiApi =
  NamiApi JSVal
  deriving (MakeObject)

-- TODO(skylar): Currently we don't have overloaded strings so "cardano" is a string
-- but eventually we might need overloaded strings...

getApi :: (MonadJSM m) => m (Either APIError NamiApi)
getApi = liftJSM $ do
  window <- currentWindowUnchecked
  promise <- window ^. js "cardano" . js "nami" . js0 "enable"
  -- pure $ Left $ APIError "Nami Wallet Not Enabled"
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
      results :: [Address] <- fromJSArrayUnchecked val
      case results of
        [addr] -> pure $ Just addr
        _ -> pure Nothing

signTest :: MonadJSM m => NamiApi -> Address -> m ()
signTest napi addr = liftJSM $ do
  _ <- napi ^. js2 "signData" addr cbor
  pure ()
  where
    message = "RPC Not detected: Testing Sign Functionality Instead, if you see this signing is working"
    cbor = toText $ fromBytes $ LBS.toStrict $ serialise $ T.pack message

-- WebAssembly.instantiateStreaming(fetch(/static/wasm/cardano_serialization_lib_bg.wasm), importObject)
testWasm :: MonadJSM m => m ()
testWasm = liftJSM $ do
  eval $ "importObject = { imports: { imported_func: arg => console.log(arg), wasi_unstable: () => {} }}"
  -- eval $ "importObject = { imports: { imported_func: arg => console.log(arg) } }"
  -- liftIO $ putStrLn $ "WebAssembly.instantiateStreaming(fetch('" <> $(static "wasm/cardano_serialization_lib_bg.wasm") <> "'), importObject)"
  -- promise <- eval $ "WebAssembly.instantiateStreaming(fetch('" <> cdn <> "'), importObject)"
  promise <- eval $ "WebAssembly.instantiateStreaming(fetch('" <> cdn <> "'), importObject)"
  result <- promiseMaybe (unsafeToPromise promise) (pure . id)
  case result of
    Just _ -> eval $ "console.log('Yay')"
    Nothing -> eval $ "console.log('Womp womp')"
  pure ()
  where
    notcdn = $(static "wasm/cardano_serialization_lib_bg.wasm")
    cdn = "https://cdn.jsdelivr.net/npm/@emurgo/cardano-serialization-lib-browser@10.0.3/cardano_serialization_lib_bg.wasm"
