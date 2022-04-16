{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: SUDT related functions, currently very experimental

module Bridge.Nervos.SUDT where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.HexString

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Basement.Types.Word128 hiding ((-))

-- TODO what is this type
newtype SUDTAmount =
  SUDTAmount { unSUDTAmount :: Word128 }
  deriving (Eq, Show, Num)

instance Binary SUDTAmount where
  put (SUDTAmount (Word128 ho lo) ) = do
    putWord64le lo
    putWord64le ho

  get =
    (\lo ho -> SUDTAmount $ Word128 ho lo) <$> getWord64le <*> getWord64le

-- TODO: not this
rejig :: T.Text -> T.Text
rejig t
  | t == "0x" = t
  | lenStr < ideallen = stripped <> (LT.toStrict $ LT.take (fromIntegral $ toInteger $ ideallen - lenStr) (LT.repeat '0'))
  | otherwise = stripped
  where
    lenStr = T.length stripped
    stripped = T.drop 2 t
    ideallen = T.length "e8030000000000000000000000000000"

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

-- TODO: Make this a maybe right away
fromHexUtf8 :: Binary a => T.Text -> Maybe a
fromHexUtf8 t = do
  here <- hexString' $ T.encodeUtf8 t
  let bytes =
        toBytes here
  case decodeOrFail $ LBS.fromStrict bytes of
    Left _ -> Nothing
    Right (_, _, a) -> Just a
