{-# LANGUAGE OverloadedStrings #-}
module Odf where

import Text.XML.HaXml.Schema.PrimitiveTypes (XsdString)

import OdfXsd


mkObjects :: ObjectsType
mkObjects = ObjectsType
        { objectsType_version = Just "1.0" :: Maybe Xs.XsdString
          -- ^ Schema version used.
        , objectsType_object = [] :: [ObjectType]
        }
 

