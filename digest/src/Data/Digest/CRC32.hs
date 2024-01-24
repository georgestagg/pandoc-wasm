{-# LANGUAGE FlexibleInstances #-}

module Data.Digest.CRC32
    ( CRC32, crc32, crc32Update
    ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Asterius.Magic
import Asterius.Types
import Asterius.ByteString

class CRC32 a where
    crc32 :: a -> Word32
    crc32 = crc32Update 0
    crc32Update :: Word32 -> a -> Word32

instance CRC32 BS.ByteString where
    crc32Update previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_crc32 (byteStringToJSUint8Array bytes) (fromIntegral $ previous)

instance CRC32 BL.ByteString where
    crc32Update previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_crc32 (byteStringToJSUint8Array . BL.toStrict $ bytes)
      (fromIntegral $ previous)

instance CRC32 [Word8] where
    crc32Update previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_crc32 (byteStringToJSUint8Array . BS.pack $ bytes)
      (fromIntegral $ previous)

foreign import javascript unsafe "Pandoc.digest.crc32.buf($1, $2)"
  js_crc32 :: JSUint8Array -> Int -> IO Int
