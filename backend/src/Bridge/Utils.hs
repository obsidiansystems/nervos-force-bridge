{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | 

module Bridge.Utils ( module E
                    , BridgeM
                    , runBridge
                    , runBridgeInFile
                    , scrubPrefix

                    , fromHexUtf8
                    , hexString'
                    ) where

import System.IO
import Control.Monad.Log as E
import Control.Monad.IO.Class as E
import qualified Data.Text as T

import Data.Binary
import Data.HexString

import qualified Data.Text.Encoding as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Prettyprinter (pretty)
import Prettyprinter.Render.Text (putDoc, hPutDoc)

import Data.Aeson

type BridgeM m = (MonadLog (WithSeverity T.Text) m, MonadIO m)

hexString' :: BS.ByteString -> Maybe HexString
hexString' bs =
  let isValidHex :: Word8 -> Bool
      isValidHex c
        | (48 <= c) && (c < 58)  = True
        | (97 <= c) && (c < 103) = True
        | otherwise              = False
  in if   BS.all isValidHex bs
     then Just (hexString bs)
     else Nothing

fromHexUtf8 :: Binary a => T.Text -> Maybe a
fromHexUtf8 t = do
  here <- hexString' $ T.encodeUtf8 t
  let bytes =
        toBytes here
  case decodeOrFail $ LBS.fromStrict bytes of
    Left _ -> Nothing
    Right (_, _, a) -> Just a

scrubPrefix :: String -> Options
scrubPrefix s =
  defaultOptions { fieldLabelModifier = drop (length s)
                 }

runBridge :: LoggingT (WithSeverity T.Text) IO a -> IO a
runBridge = flip runLoggingT (putDoc . renderWithSeverity pretty)

runBridgeInFile :: FilePath -> LoggingT (WithSeverity T.Text) IO a -> IO a
runBridgeInFile fp action = withFile fp AppendMode $ (\h ->
  flip runLoggingT (\x -> (hPutDoc h $ renderWithSeverity pretty x) >> hPutStrLn h "" >> hFlush h) action)

runBridgeNoLogging :: LoggingT (WithSeverity T.Text) IO a -> IO a
runBridgeNoLogging = flip runLoggingT (const $ pure ())
