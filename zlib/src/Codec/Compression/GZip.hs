module Codec.Compression.GZip
    ( compress
    , decompress
    ) where

import qualified Data.ByteString.Lazy as BL
import Asterius.Magic
import Asterius.Types
import Asterius.ByteString

decompress :: BL.ByteString -> BL.ByteString
decompress = BL.fromStrict . byteStringFromJSUint8Array
  . accursedUnutterablePerformIO . js_decompress
  . byteStringToJSUint8Array . BL.toStrict

compress :: BL.ByteString -> BL.ByteString
compress = BL.fromStrict . byteStringFromJSUint8Array
  . accursedUnutterablePerformIO . js_compress
  . byteStringToJSUint8Array . BL.toStrict

foreign import javascript unsafe "Pandoc.pako.ungzip($1)"
  js_decompress :: JSUint8Array -> IO JSUint8Array

foreign import javascript unsafe "Pandoc.pako.gzip($1)"
  js_compress :: JSUint8Array -> IO JSUint8Array
