{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
module OdfXsd
  ( module OdfXsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),SimpleType(..),Extension(..),Restricts(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.OneOfN
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
 
-- Some hs-boot imports are required, for fwd-declaring types.
 
elementObject :: XMLParser ObjectType
elementObject = parseSchemaType "Object"
elementToXMLObject :: ObjectType -> [Content ()]
elementToXMLObject = schemaTypeToXML "Object"
 
elementInfoItem :: XMLParser InfoItemType
elementInfoItem = parseSchemaType "InfoItem"
elementToXMLInfoItem :: InfoItemType -> [Content ()]
elementToXMLInfoItem = schemaTypeToXML "InfoItem"
 
elementValue :: XMLParser ValueType
elementValue = parseSchemaType "value"
elementToXMLValue :: ValueType -> [Content ()]
elementToXMLValue = schemaTypeToXML "value"
 
-- | Data Model Root Element
elementObjects :: XMLParser ObjectsType
elementObjects = parseSchemaType "Objects"
elementToXMLObjects :: ObjectsType -> [Content ()]
elementToXMLObjects = schemaTypeToXML "Objects"
 
data ObjectsType = ObjectsType
        { objectsType_version :: Maybe Xs.XsdString
          -- ^ Schema version used.
        , objectsType_object :: [ObjectType]
        }
        deriving (Eq,Show)
instance SchemaType ObjectsType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "version" e pos
        commit $ interior e $ return (ObjectsType a0)
            `apply` many (elementObject)
    schemaTypeToXML s x@ObjectsType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "version") $ objectsType_version x
                       ]
            [ concatMap (elementToXMLObject) $ objectsType_object x
            ]
 
data ObjectType = ObjectType
        { objectType_type :: Maybe Xs.XsdString
        , objectType_id :: [QlmID]
        , objectType_description :: Maybe Description
          -- ^ String with some"human-readable" text.
        , objectType_infoItem :: [InfoItemType]
        , objectType_object :: [ObjectType]
        }
        deriving (Eq,Show)
instance SchemaType ObjectType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- optional $ getAttribute "type" e pos
        commit $ interior e $ return (ObjectType a0)
            `apply` many1 (parseSchemaType "id")
            `apply` optional (elementDescription)
            `apply` many (elementInfoItem)
            `apply` many (elementObject)
    schemaTypeToXML s x@ObjectType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "type") $ objectType_type x
                       ]
            [ concatMap (schemaTypeToXML "id") $ objectType_id x
            , maybe [] (elementToXMLDescription) $ objectType_description x
            , concatMap (elementToXMLInfoItem) $ objectType_infoItem x
            , concatMap (elementToXMLObject) $ objectType_object x
            ]
 
data InfoItemType = InfoItemType
        { infoItemType_nameAttr :: Xs.XsdString
          -- ^ Name of InfoItem, such as "PowerConsumption", "Diameter" or 
          --   similar.
        , infoItemType_name :: [QlmID]
          -- ^ Optional list of other names for the same InfoItem.
        , infoItemType_description :: Maybe Description
          -- ^ String with some"human-readable" text.
        , infoItemType_metaData :: Maybe MetaDataType
          -- ^ Meta-data about the InfoItem, such as "latency", "unit" 
          --   etc.
        , infoItemType_value :: [ValueType]
        }
        deriving (Eq,Show)
instance SchemaType InfoItemType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        a0 <- getAttribute "name" e pos
        commit $ interior e $ return (InfoItemType a0)
            `apply` many (parseSchemaType "name")
            `apply` optional (elementDescription)
            `apply` optional (parseSchemaType "MetaData")
            `apply` many (elementValue)
    schemaTypeToXML s x@InfoItemType{} =
        toXMLElement s [ toXMLAttribute "name" $ infoItemType_nameAttr x
                       ]
            [ concatMap (schemaTypeToXML "name") $ infoItemType_name x
            , maybe [] (elementToXMLDescription) $ infoItemType_description x
            , maybe [] (schemaTypeToXML "MetaData") $ infoItemType_metaData x
            , concatMap (elementToXMLValue) $ infoItemType_value x
            ]

-- | Meta-data about the InfoItem, such as "latency", "unit"                                                                                                                                                                                                              
--   etc.                                                                                                                                                                                                                                                                 
elementMetaData :: XMLParser MetaDataType                                                                                                                                                                                                                                 
elementMetaData = parseSchemaType "MetaData"                                                                                                                                                                                                                              
elementToXMLMetaData :: MetaDataType -> [Content ()]                                                                                                                                                                                                                      
elementToXMLMetaData = schemaTypeToXML "MetaData" 

data MetaDataType = MetaDataType                                                                                                                                                                                                                                          
        { metaDataType_infoItem :: [InfoItemType]                                                                                                                                                                                                                         
                }                                                                                                                                                                                                                                                                 
                        deriving (Eq,Show)                                                                                                                                                                                                                                                
instance SchemaType MetaDataType where                                                                                                                                                                                                                                    
    parseSchemaType s = do                                                                                                                                                                                                                                                
        (pos,e) <- posnElement [s]                                                                                                                                                                                                                                        
        commit $ interior e $ return MetaDataType                                                                                                                                                                                                                         
            `apply` many (elementInfoItem)                                                                                                                                                                                                                                
    schemaTypeToXML s x@MetaDataType{} =                                                                                                                                                                                                                                  
        toXMLElement s []                                                                                                                                                                                                                                                 
            [ concatMap (elementToXMLInfoItem) $ metaDataType_infoItem x                                                                                                                                                                                                  
            ]
 
-- | String with some"human-readable" text.
data Description = Description
        deriving (Eq,Show)
instance SchemaType Description where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ interior e $ return Description
    schemaTypeToXML s x@Description{} =
        toXMLElement s []
            []
 
-- | String with some"human-readable" text.
elementDescription :: XMLParser Description
elementDescription = parseSchemaType "description"
elementToXMLDescription :: Description -> [Content ()]
elementToXMLDescription = schemaTypeToXML "description"
 
data QlmID = QlmID Xs.XsdString QlmIDAttributes deriving (Eq,Show)
data QlmIDAttributes = QlmIDAttributes
    { qlmIDAttributes_idType :: Maybe Xs.XsdString
      -- ^ Text identifying the ID schema.
    , qlmIDAttributes_tagType :: Maybe Xs.XsdString
      -- ^ Text identifying the ID Tag media type.
    , qlmIDAttributes_startDate :: Maybe Xs.DateTime
      -- ^ Start of validity for the ID
    , qlmIDAttributes_endDate :: Maybe Xs.DateTime
      -- ^ End of validity for the ID
    }
    deriving (Eq,Show)
instance SchemaType QlmID where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "idType" e pos
          a1 <- optional $ getAttribute "tagType" e pos
          a2 <- optional $ getAttribute "startDate" e pos
          a3 <- optional $ getAttribute "endDate" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ QlmID v (QlmIDAttributes a0 a1 a2 a3)
    schemaTypeToXML s (QlmID bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "idType") $ qlmIDAttributes_idType at
                         , maybe [] (toXMLAttribute "tagType") $ qlmIDAttributes_tagType at
                         , maybe [] (toXMLAttribute "startDate") $ qlmIDAttributes_startDate at
                         , maybe [] (toXMLAttribute "endDate") $ qlmIDAttributes_endDate at
                         ]
            $ schemaTypeToXML s bt
instance Extension QlmID Xs.XsdString where
    supertype (QlmID s _) = s
 
data ValueType = ValueType Xs.XsdString ValueTypeAttributes deriving (Eq,Show)
data ValueTypeAttributes = ValueTypeAttributes
    { valueTypeAttributes_type :: Maybe Xs.XsdString
    , valueTypeAttributes_dateTime :: Maybe Xs.DateTime
    , valueTypeAttributes_unixTime :: Maybe Xs.Long
    }
    deriving (Eq,Show)
instance SchemaType ValueType where
    parseSchemaType s = do
        (pos,e) <- posnElement [s]
        commit $ do
          a0 <- optional $ getAttribute "type" e pos
          a1 <- optional $ getAttribute "dateTime" e pos
          a2 <- optional $ getAttribute "unixTime" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ ValueType v (ValueTypeAttributes a0 a1 a2)
    schemaTypeToXML s (ValueType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "type") $ valueTypeAttributes_type at
                         , maybe [] (toXMLAttribute "dateTime") $ valueTypeAttributes_dateTime at
                         , maybe [] (toXMLAttribute "unixTime") $ valueTypeAttributes_unixTime at
                         ]
            $ schemaTypeToXML s bt
instance Extension ValueType Xs.XsdString where
    supertype (ValueType s _) = s
