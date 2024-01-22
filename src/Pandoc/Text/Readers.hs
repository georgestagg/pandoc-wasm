{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections       #-}

module Text.Readers
  (   readersMinimal
    , getReaderFromList
  )
where

import Control.Monad.Except (throwError)

import Data.Text

import Text.Pandoc
import Text.Pandoc.Readers.Man (readMan)
import qualified Text.Pandoc.Format as Format

readersMinimal :: PandocMonad m => [(Text, Reader m)]
readersMinimal =  [("native"       , TextReader readNative)
                  ,("json"         , TextReader readJSON)
                  ,("markdown"     , TextReader readMarkdown)
                  ,("markdown_strict" , TextReader readMarkdown)
                  ,("markdown_phpextra" , TextReader readMarkdown)
                  ,("markdown_github" , TextReader readMarkdown)
                  ,("markdown_mmd",  TextReader readMarkdown)
                  ,("commonmark"   , TextReader readCommonMark)
                  ,("commonmark_x" , TextReader readCommonMark)
                  ,("creole"       , TextReader readCreole)
                  ,("dokuwiki"     , TextReader readDokuWiki)
                  ,("gfm"          , TextReader readCommonMark)
                  ,("rst"          , TextReader readRST)
                  ,("mediawiki"    , TextReader readMediaWiki)
                  ,("vimwiki"      , TextReader readVimwiki)
                  ,("docbook"      , TextReader readDocBook)
                  ,("opml"         , TextReader readOPML)
                  ,("org"          , TextReader readOrg)
                  ,("textile"      , TextReader readTextile)
                  ,("html"         , TextReader readHtml)
                  ,("bits"         , TextReader readJATS)
                  ,("jats"         , TextReader readJATS)
                  ,("jira"         , TextReader readJira)
                  ,("latex"        , TextReader readLaTeX)
                  ,("haddock"      , TextReader readHaddock)
                  ,("twiki"        , TextReader readTWiki)
                  ,("tikiwiki"     , TextReader readTikiWiki)
                  ,("docx"         , ByteStringReader readDocx)
                  ,("odt"          , ByteStringReader readODT)
                  ,("t2t"          , TextReader readTxt2Tags)
                  ,("epub"         , ByteStringReader readEPUB)
                  ,("muse"         , TextReader readMuse)
                  ,("man"          , TextReader readMan)
                  ,("fb2"          , TextReader readFB2)
                  ,("ipynb"        , TextReader readIpynb)
                  ,("csv"          , TextReader readCSV)
                  ,("tsv"          , TextReader readTSV)
                  ,("csljson"      , TextReader readCslJson)
                  ,("bibtex"       , TextReader readBibTeX)
                  ,("biblatex"     , TextReader readBibLaTeX)
                  ,("endnotexml"   , TextReader readEndNoteXML)
                  ,("ris"          , TextReader readRIS)
                  ,("rtf"          , TextReader readRTF)
                  -- ,("typst"        , TextReader readTypst)
                   ]

getReaderFromList :: PandocMonad m => [(Text, Reader m)]
                      -> Format.FlavoredFormat -> m (Reader m, Extensions)
getReaderFromList readerList flvrd = do
  let readerName = Format.formatName flvrd
  case lookup readerName readerList of
    Nothing  -> throwError $ PandocUnknownReaderError readerName
    Just  r  -> (r,) <$>
      Format.applyExtensionsDiff (Format.getExtensionsConfig readerName) flvrd

