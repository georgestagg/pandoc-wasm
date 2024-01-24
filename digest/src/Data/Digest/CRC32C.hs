{-# LANGUAGE FlexibleInstances #-}

module Data.Digest.CRC32C
    ( CRC32C, crc32c, crc32cUpdate
    ) where

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Asterius.Magic
import Asterius.Types
import Asterius.ByteString

class CRC32C a where
    crc32c :: a -> Word32
    crc32c = crc32cUpdate 0
    crc32cUpdate :: Word32 -> a -> Word32

instance CRC32C BS.ByteString where
    crc32cUpdate previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_crc32c (byteStringToJSUint8Array bytes) (fromIntegral $ previous)

instance CRC32C BL.ByteString where
    crc32cUpdate previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_crc32c (byteStringToJSUint8Array . BL.toStrict $ bytes)
      (fromIntegral $ previous)

instance CRC32C [Word8] where
    crc32cUpdate previous bytes = fromIntegral . accursedUnutterablePerformIO
      $ js_crc32c (byteStringToJSUint8Array . BS.pack $ bytes)
      (fromIntegral $ previous)

foreign import javascript unsafe "Pandoc.digest.crc32c.buf($1, $2)"
  js_crc32c :: JSUint8Array -> Int -> IO Int
