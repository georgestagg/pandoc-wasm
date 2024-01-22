-- Based on pandoc/pandoc-server/src/Text/Pandoc/Server.hs
-- Released under the GPL, version 2 or later, see pandoc/COPYRIGHT for details.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import Asterius.Aeson (jsonFromJSVal', jsonToJSVal)
import Asterius.Text (textToJSString, textFromJSString)
import Asterius.Types
import Control.Monad.Except
import Data.Aeson
import Data.Default
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import GHC.Generics

import Text.Pandoc
import Text.Pandoc.App ( IpynbOutput (..), Opt(..), defaultOpts )
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Shared (safeStrRead, headerShift, filterIpynbOutput,
                           eastAsianLineBreakFilter)
import Text.Pandoc.Format (parseFlavoredFormat, formatName)
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Readers (getReaderFromList, readersMinimal)
import Text.Writers (getWriterFromList, writersMinimal)
import Text.Pandoc.Highlighting (lookupHighlightingStyle)
import Skylighting (defaultSyntaxMap)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64 (decodeLenient, encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

newtype Blob = Blob BL.ByteString
  deriving (Show, Eq)

instance ToJSON Blob where
  toJSON (Blob bs) = toJSON (UTF8.toText . Base64.encode $ BL.toStrict bs)

instance FromJSON Blob where
 parseJSON = withText "Blob" $
   pure . Blob . BL.fromStrict . Base64.decodeLenient . UTF8.fromText

data Params = Params
  { options               :: Opt
  , text                  :: Text
  , files                 :: Maybe (M.Map FilePath Blob)
  , citeproc              :: Maybe Bool
  } deriving (Show)

instance Default Params where
  def = Params
    { options = defaultOpts
    , text = mempty
    , files = Nothing
    , citeproc = Nothing
    }

instance FromJSON Params where
 parseJSON = withObject "Params" $ \o ->
   Params
     <$> o .: "options"
     <*> o .: "text"
     <*> o .:? "files"
     <*> o .:? "citeproc"

data Output = Succeeded Text
            | Failed Text
  deriving (Generic, Show)

instance ToJSON Output where
  toEncoding (Succeeded o) = pairs
    ( "output" .= o )
  toEncoding (Failed errmsg) = pairs
    ( "error" .= errmsg )

main :: IO ()
main = error "This application can't be executed by its main entrypoint."

getVersion :: JSString
getVersion = textToJSString pandocVersionText

runPandoc :: JSVal -> JSVal
runPandoc input = jsonToJSVal . convert . jsonFromJSVal' $ input

convert :: Params -> Output
convert params = handleErr . runPure $ convert' (return)
                  (return . UTF8.toText . Base64.encode . BL.toStrict) params

convert' :: (Text -> PandocPure Text) -> (BL.ByteString -> PandocPure Text)
              -> Params -> PandocPure Text
convert' textHandler bsHandler params = do
    curtime <- getCurrentTime

    let addFile :: FilePath -> Blob -> FileTree -> FileTree
        addFile fp (Blob lbs) =
          insertInFileTree fp FileInfo{ infoFileMTime = curtime
                                      , infoFileContents = BL.toStrict lbs }
    case files params of
      Nothing -> return ()
      Just fs -> do
        let filetree = M.foldrWithKey addFile mempty fs
        modifyPureState $ \st -> st{ stFiles = filetree }

    let opts = options params
    readerFormat <- parseFlavoredFormat <$> fromMaybe "markdown" $ optFrom opts
    writerFormat <- parseFlavoredFormat <$> fromMaybe "html" $ optTo opts
    (readerSpec, readerExts) <- getReaderFromList readersMinimal readerFormat
    (writerSpec, writerExts) <- getWriterFromList writersMinimal writerFormat

    let isStandalone = optStandalone opts
    let toformat = formatName writerFormat
    hlStyle <- traverse (lookupHighlightingStyle . T.unpack)
                  $ optHighlightStyle opts

    mbTemplate <- if isStandalone
                     then case optTemplate opts of
                            Nothing -> Just <$>
                              compileDefaultTemplate toformat
                            Just t  -> Just <$>
                              compileCustomTemplate toformat t
                     else return Nothing

    abbrevs <- Set.fromList . filter (not . T.null) . T.lines . UTF8.toText <$>
                 case optAbbreviations opts of
                      Nothing -> readDataFile "abbreviations"
                      Just f  -> readFileStrict f

    let readeropts = def{ readerExtensions = readerExts
                        , readerStandalone = isStandalone
                        , readerTabStop = optTabStop opts
                        , readerIndentedCodeClasses =
                            optIndentedCodeClasses opts
                        , readerAbbreviations = abbrevs
                        , readerDefaultImageExtension =
                            optDefaultImageExtension opts
                        , readerTrackChanges = optTrackChanges opts
                        , readerStripComments = optStripComments opts
                        }

    let writeropts =
          def{ writerExtensions = writerExts
             , writerTabStop = optTabStop opts
             , writerWrapText = optWrap opts
             , writerColumns = optColumns opts
             , writerTemplate = mbTemplate
             , writerSyntaxMap = defaultSyntaxMap
             , writerVariables = optVariables opts
             , writerTableOfContents = optTableOfContents opts
             , writerIncremental = optIncremental opts
             , writerHTMLMathMethod = optHTMLMathMethod opts
             , writerNumberSections = optNumberSections opts
             , writerNumberOffset = optNumberOffset opts
             , writerSectionDivs = optSectionDivs opts
             , writerReferenceLinks = optReferenceLinks opts
             , writerDpi = optDpi opts
             , writerEmailObfuscation = optEmailObfuscation opts
             , writerIdentifierPrefix = optIdentifierPrefix opts
             , writerCiteMethod = optCiteMethod opts
             , writerHtmlQTags = optHtmlQTags opts
             , writerSlideLevel = optSlideLevel opts
             , writerTopLevelDivision = optTopLevelDivision opts
             , writerListings = optListings opts
             , writerHighlightStyle = hlStyle
             , writerSetextHeaders = optSetextHeaders opts
             , writerEpubSubdirectory = T.pack $ optEpubSubdirectory opts
             , writerEpubMetadata = T.pack <$> optEpubMetadata opts
             , writerEpubFonts = optEpubFonts opts
             , writerSplitLevel = optSplitLevel opts
             , writerTOCDepth = optTOCDepth opts
             , writerReferenceDoc = optReferenceDoc opts
             , writerReferenceLocation = optReferenceLocation opts
             , writerPreferAscii = optAscii opts
             }

    let reader = case readerSpec of
                TextReader r -> r readeropts
                ByteStringReader r ->
                  r readeropts . BL.fromStrict . Base64.decodeLenient
                    . UTF8.fromText

    let writer d@(Pandoc meta _) = do
          case writerSpec of
                TextWriter w ->
                  w writeropts d >>=
                    (if optEmbedResources opts && htmlFormat (optTo opts)
                        then makeSelfContained
                        else return) >>=
                    textHandler
                ByteStringWriter w ->
                  w writeropts d >>= bsHandler

    let transforms :: Pandoc -> Pandoc
        transforms = (case optShiftHeadingLevelBy opts of
                        0             -> id
                        x             -> headerShift x) .
                   (if extensionEnabled Ext_east_asian_line_breaks
                          readerExts &&
                       not (extensionEnabled Ext_east_asian_line_breaks
                              writerExts &&
                            optWrap opts == WrapPreserve)
                       then eastAsianLineBreakFilter
                       else id) .
                   (case optIpynbOutput opts of
                     IpynbOutputAll  -> id
                     IpynbOutputNone -> filterIpynbOutput Nothing
                     IpynbOutputBest -> filterIpynbOutput (Just $
                       case optTo opts of
                            Just "latex"  -> Format "latex"
                            Just "beamer" -> Format "latex"
                            Nothing       -> Format "html"
                            Just f
                              | htmlFormat (optTo opts) -> Format "html"
                              | otherwise -> Format f))

    let meta =   (case optBibliography opts of
                   [] -> id
                   fs -> setMeta "bibliography" (MetaList
                            (map (MetaString . T.pack) fs))) .
                 maybe id (setMeta "csl" . MetaString . T.pack)
                   (optCSL opts) .
                 maybe id (setMeta "citation-abbreviations" . MetaString .
                              T.pack)
                   (optCitationAbbreviations opts) $
                 optMetadata opts

    let addMetadata m' (Pandoc m bs) = Pandoc (m <> m') bs

    reader (text params) >>=
      return . transforms . addMetadata meta >>=
        (case citeproc params of
          Just True -> processCitations
          _ -> return) >>=
      writer

handleErr :: Either PandocError Text -> Output
handleErr (Right t) = Succeeded t
handleErr (Left err) = Failed (renderError err)

htmlFormat :: Maybe Text -> Bool
htmlFormat Nothing = True
htmlFormat (Just f) =
  any (`T.isPrefixOf` f)
    ["html","html4","html5","s5","slidy", "slideous","dzslides","revealjs"]

compileCustomTemplate toformat t = do
  res <- runWithPartials $ compileTemplate ("custom." <> T.unpack toformat)
              (T.pack t)
  case res of
    Left e -> throwError $ PandocTemplateError (T.pack e)
    Right tpl -> return tpl

foreign export javascript "runPandoc" runPandoc :: JSVal -> JSVal
foreign export javascript "getVersion" getVersion :: JSString
