{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util.HaXml where

import Data.String (IsString(..))

import Text.XML.HaXml.Types (Document(..))
import Text.XML.HaXml.Parse (xmlParse')
import Text.XML.HaXml.Posn (posInNewCxt)
import Text.XML.HaXml.Schema.Schema (XMLParser, runParser, Content(CElem))

import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs

instance IsString Xs.XsdString where
    fromString = Xs.XsdString

parseFile :: XMLParser t -> String -> IO (Either String t)
parseFile parser fname = do
    xmlText <- readFile fname
    return $ parseText parser xmlText

parseText :: XMLParser t -> String -> Either String t
parseText = parseText' "<memory>"

parseText' :: String -> XMLParser t -> String -> Either String t
parseText' xmlFileString parser xmlText = do
    (Document _ _ root _) <- xmlParse' xmlFileString xmlText
    fst $ runParser parser [CElem root (posInNewCxt xmlFileString Nothing)]

