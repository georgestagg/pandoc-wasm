{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Codec.Compression.Zlib
    ( compress
    , decompress
    , WindowBits(..)
      , defaultWindowBits
      , windowBits
    ) where

import GHC.Generics
import Data.Typeable
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.Zlib.Raw as Raw

decompress :: BL.ByteString -> BL.ByteString
decompress = Raw.decompress

compress :: BL.ByteString -> BL.ByteString
compress = Raw.compress

data WindowBits = WindowBits Int | DefaultWindowBits
  deriving ( Eq, Ord , Show , Typeable, Generic )

defaultWindowBits :: WindowBits
defaultWindowBits = WindowBits 15

windowBits :: Int -> WindowBits
windowBits n = WindowBits n
