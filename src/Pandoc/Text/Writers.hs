{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections       #-}

module Text.Writers
  (   writersMinimal
    , getWriterFromList
  )
where

import Control.Monad.Except (throwError)

import Data.Text

import Text.Pandoc
import qualified Text.Pandoc.Format as Format

writersMinimal :: PandocMonad m => [ (Text, Writer m) ]
writersMinimal =  [("native"       , TextWriter writeNative)
                  ,("json"         , TextWriter writeJSON)
                  ,("docx"         , ByteStringWriter writeDocx)
                  ,("odt"          , ByteStringWriter writeODT)
                  ,("pptx"         , ByteStringWriter writePowerpoint)
                  ,("epub"         , ByteStringWriter writeEPUB3)
                  ,("epub2"        , ByteStringWriter writeEPUB2)
                  ,("epub3"        , ByteStringWriter writeEPUB3)
                  ,("fb2"          , TextWriter writeFB2)
                  ,("ipynb"        , TextWriter writeIpynb)
                  ,("html"         , TextWriter writeHtml5String)
                  ,("html4"        , TextWriter writeHtml4String)
                  ,("html5"        , TextWriter writeHtml5String)
                  ,("icml"         , TextWriter writeICML)
                  ,("s5"           , TextWriter writeS5)
                  ,("slidy"        , TextWriter writeSlidy)
                  ,("slideous"     , TextWriter writeSlideous)
                  ,("dzslides"     , TextWriter writeDZSlides)
                  ,("revealjs"     , TextWriter writeRevealJs)
                  ,("docbook"      , TextWriter writeDocBook5)
                  ,("docbook4"     , TextWriter writeDocBook4)
                  ,("docbook5"     , TextWriter writeDocBook5)
                  ,("jats"         , TextWriter writeJatsArchiving)
                  ,("jats_articleauthoring", TextWriter writeJatsArticleAuthoring)
                  ,("jats_publishing" , TextWriter writeJatsPublishing)
                  ,("jats_archiving" , TextWriter writeJatsArchiving)
                  ,("jira"         , TextWriter writeJira)
                  ,("opml"         , TextWriter writeOPML)
                  ,("opendocument" , TextWriter writeOpenDocument)
                  ,("latex"        , TextWriter writeLaTeX)
                  ,("beamer"       , TextWriter writeBeamer)
                  ,("context"      , TextWriter writeConTeXt)
                  ,("texinfo"      , TextWriter writeTexinfo)
                  ,("man"          , TextWriter writeMan)
                  ,("ms"           , TextWriter writeMs)
                  ,("markdown"     , TextWriter writeMarkdown)
                  ,("markdown_strict" , TextWriter writeMarkdown)
                  ,("markdown_phpextra" , TextWriter writeMarkdown)
                  ,("markdown_github" , TextWriter writeMarkdown)
                  ,("markdown_mmd" , TextWriter writeMarkdown)
                  ,("plain"        , TextWriter writePlain)
                  ,("rst"          , TextWriter writeRST)
                  ,("mediawiki"    , TextWriter writeMediaWiki)
                  ,("dokuwiki"     , TextWriter writeDokuWiki)
                  ,("xwiki"        , TextWriter writeXWiki)
                  ,("zimwiki"      , TextWriter writeZimWiki)
                  ,("textile"      , TextWriter writeTextile)
                  ,("typst"        , TextWriter writeTypst)
                  ,("rtf"          , TextWriter writeRTF)
                  ,("org"          , TextWriter writeOrg)
                  ,("asciidoc"     , TextWriter writeAsciiDoc)
                  ,("asciidoctor"  , TextWriter writeAsciiDoc)
                  ,("asciidoc_legacy" , TextWriter writeAsciiDocLegacy)
                  ,("haddock"      , TextWriter writeHaddock)
                  ,("commonmark"   , TextWriter writeCommonMark)
                  ,("commonmark_x" , TextWriter writeCommonMark)
                  ,("gfm"          , TextWriter writeCommonMark)
                  ,("tei"          , TextWriter writeTEI)
                  ,("muse"         , TextWriter writeMuse)
                  ,("csljson"      , TextWriter writeCslJson)
                  ,("bibtex"       , TextWriter writeBibTeX)
                  ,("biblatex"     , TextWriter writeBibLaTeX)
                  ,("markua"       , TextWriter writeMarkua)
                  ,("chunkedhtml"  , ByteStringWriter writeChunkedHTML)
                   ]

getWriterFromList :: PandocMonad m => [ (Text, Writer m) ]
                      -> Format.FlavoredFormat -> m (Writer m, Extensions)
getWriterFromList writerList flvrd = do
  let writerName = Format.formatName flvrd
  case lookup writerName writerList of
    Nothing  -> throwError $ PandocUnknownWriterError writerName
    Just  w  -> (w,) <$>
      Format.applyExtensionsDiff (Format.getExtensionsConfig writerName) flvrd
