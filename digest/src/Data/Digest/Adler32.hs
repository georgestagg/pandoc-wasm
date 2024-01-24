{-# LANGUAGE FlexibleInstances #-}

module Data.Digest.Adler32
    ( Adler32, adler32, adler32Update
    ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Asterius.Magic
import Asterius.Types
import Asterius.ByteString

class Adler32 a where
    adler32 :: a -> Word32
    adler32 = adler32Update 0
    adler32Update :: Word32 -> a -> Word32

instance Adler32 BS.ByteString where
    adler32Update previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_adler32 (byteStringToJSUint8Array bytes) (fromIntegral $ previous)

instance Adler32 BL.ByteString where
    adler32Update previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_adler32 (byteStringToJSUint8Array . BL.toStrict $ bytes)
      (fromIntegral $ previous)

instance Adler32 [Word8] where
    adler32Update previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_adler32 (byteStringToJSUint8Array . BS.pack $ bytes)
      (fromIntegral $ previous)

foreign import javascript unsafe "Pandoc.digest.adler32.buf($1, $2)"
  js_adler32 :: JSUint8Array -> Int -> IO Int
