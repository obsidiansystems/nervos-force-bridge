{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Description: SUDT related functions, currently very experimental

module Bridge.Nervos.SUDT where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

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
