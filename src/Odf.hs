{-# LANGUAGE OverloadedStrings #-}
module Odf where

import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs

import OdfXsd
import Util.HaXml()

newtype Path = Path String

mkObjects :: ObjectsType
mkObjects = ObjectsType
        { objectsType_version = Just "1.0" :: Maybe Xs.XsdString
          -- ^ Schema version used.
        , objectsType_object = [] :: [ObjectType]
        }
 
type PathValues = [(Path, String)]

flattenOdf :: ObjectsType -> PathValues
flattenOdf = undefined

