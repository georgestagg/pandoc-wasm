module Codec.Compression.Zlib.Raw
    ( compress
    , decompress
    ) where

import qualified Data.ByteString.Lazy as BL
import Asterius.Magic
import Asterius.Types
import Asterius.ByteString

decompress :: BL.ByteString -> BL.ByteString
decompress = BL.fromStrict . byteStringFromJSUint8Array
  . accursedUnutterablePerformIO . js_inflate
  . byteStringToJSUint8Array . BL.toStrict

compress :: BL.ByteString -> BL.ByteString
compress = BL.fromStrict . byteStringFromJSUint8Array
  . accursedUnutterablePerformIO . js_deflate
  . byteStringToJSUint8Array . BL.toStrict

foreign import javascript unsafe "Pandoc.pako.inflateRaw($1)"
  js_inflate :: JSUint8Array -> IO JSUint8Array

foreign import javascript unsafe "Pandoc.pako.deflateRaw($1)"
  js_deflate :: JSUint8Array -> IO JSUint8Array
