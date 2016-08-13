module Util.HaXml where

import Text.XML.HaXml.Types (Document(..))
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn (posInNewCxt)
import Text.XML.HaXml.Schema.Schema (XMLParser, runParser, Content(CElem))

parseFile :: XMLParser t -> String -> IO (Either String t)
parseFile parser fname = do
    xmlText <- readFile fname
    return $ parseText parser xmlText

parseText :: XMLParser t -> String -> Either String t
parseText parser xmlText = do
    (Document _ _ root _) <- xmlParse' xmlfileString xmlText
    fst $ runParser parser [CElem root (posInNewCxt xmlfileString Nothing)]
  where
      xmlfileString = "<memory>"


