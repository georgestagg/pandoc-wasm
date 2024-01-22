{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Data.Yaml
    ( decodeEither'
    , decodeAllEither'
    , ParseException(..)
    , prettyPrintParseException
    , ToJSON (..)
    , FromJSON (..)
    , Value (..)
    , Parser
    , Object
    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- Asterius FFI
import Asterius.Magic
import Asterius.Types
import Asterius.Text (textToJSString, textFromJSString)

data ParseException = JSYamlException String
  deriving (Generic, Show)

decodeEither' :: FromJSON a => BS.ByteString -> Either ParseException a
decodeEither' = wrapResult . eitherDecode' . BL.fromStrict . (load_ load)

decodeAllEither' :: FromJSON a => BS.ByteString -> Either ParseException [a]
decodeAllEither' = wrapResult . eitherDecode' . BL.fromStrict . (load_ loadAll)

prettyPrintParseException :: ParseException -> String
prettyPrintParseException (JSYamlException err) = err

wrapResult :: Either String a -> Either ParseException a
wrapResult (Right obj) = Right obj
wrapResult (Left errText) = Left $ JSYamlException errText

load_ :: (JSString -> IO JSString) -> BS.ByteString -> BS.ByteString
load_ fn = T.encodeUtf8 . textFromJSString . accursedUnutterablePerformIO
  . fn . textToJSString . T.decodeUtf8

foreign import javascript unsafe "JSON.stringify(Pandoc.yaml.load($1))"
  load :: JSString -> IO JSString

foreign import javascript unsafe "JSON.stringify(Pandoc.yaml.loadAll($1))"
  loadAll :: JSString -> IO JSString
