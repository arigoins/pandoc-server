{-# LANGUAGE TemplateHaskell #-}

module Processor.Simple where

import           Data.Aeson
import           Data.Aeson.TH
import           Servant
import           Text.Pandoc
import           Text.Pandoc.Shared

data RawNote = RawNote
  { rawContent :: String
  } deriving (Show)

data ProcessedNote = ProcessedNote
  { title           :: String
  , authors         :: [String]
  , renderedContent :: String
  } deriving (Show)

$(deriveJSON defaultOptions ''RawNote)
$(deriveJSON defaultOptions ''ProcessedNote)

type SimpleAPI = "process"
                 :> "md"
                 :> ReqBody '[JSON] RawNote
                 :> Post '[JSON] ProcessedNote

processHandler :: RawNote -> Handler ProcessedNote
processHandler rn = return (processNote rn)

processNote :: RawNote -> ProcessedNote
processNote rn = ProcessedNote title authors rContent
  where
  doc = readRawNote rn
  rContent = writeHTML doc
  title = getTitle doc
  authors = getAuthors doc

readRawNote :: RawNote -> Pandoc
readRawNote (RawNote rc) = case readMarkdown def rc of
  Left e -> error (show e) -- TODO: handle error better
  Right doc -> doc

writeHTML :: Pandoc -> String
writeHTML doc = writeHtmlString def doc

getTitle :: Pandoc -> String
getTitle (Pandoc meta _) = stringify (docTitle meta)

getAuthors :: Pandoc -> [String]
getAuthors (Pandoc meta _) = map stringify (docAuthors meta)
