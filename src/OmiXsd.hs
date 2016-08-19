{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports -fno-warn-unused-matches -fno-warn-orphans #-}
module OmiXsd
  ( module OmiXsd
  ) where
 
import Text.XML.HaXml.Schema.Schema (SchemaType(..),Extension(..))
import Text.XML.HaXml.Schema.Schema as Schema
import Text.XML.HaXml.Posn (Posn)
import Text.XML.HaXml.Types (Element)
import Text.XML.HaXml.OneOfN()
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs

import OdfXsd
 

-- Some hs-boot imports are required, for fwd-declaring types.


class GetOneOf a where
    getOneOf :: a -> OneOf4 ReadRequest WriteRequest ResponseListType CancelRequest

-- | takes the request and ttl
wrapToOmiEnvelope :: (GetOneOf a) => a -> Double -> OmiEnvelope
wrapToOmiEnvelope request ttl =
    OmiEnvelope (Xs.XsdString "1.0") (Xs.XsdString $ show ttl) (getOneOf request)
    
-- | A quick hack to include namespaces that are defined with name "omi"
posnElement' :: [String] -> XMLParser (Posn, Element Posn)
posnElement' = posnElement . concatMap (\s -> ["omi:"++s, s]) 

 
-- | Root element of Open Messaging Interface.
data OmiEnvelope = OmiEnvelope
        { omiEnvelope_version :: Xs.XsdString
          -- ^ Open messaging interface schema version that the message 
          --   complies with.
        , omiEnvelope_ttl :: Xs.XsdString
          -- ^ Time-to-live in seconds. "-1" signifies "forever"
        , omiEnvelope_choice0 :: OneOf4 ReadRequest WriteRequest ResponseListType CancelRequest
          -- ^ Choice between:
          --   
          --   (1) read
          --   
          --   (2) write
          --   
          --   (3) response
          --   
          --   (4) cancel
        }
        deriving (Eq,Show)
instance SchemaType OmiEnvelope where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        a0 <- getAttribute "version" e pos
        a1 <- getAttribute "ttl" e pos
        commit $ interior e $ return (OmiEnvelope a0 a1)
            `apply` oneOf' [ ("ReadRequest", fmap OneOf4 (parseSchemaType "read"))
                           , ("WriteRequest", fmap TwoOf4 (parseSchemaType "write"))
                           , ("ResponseListType", fmap ThreeOf4 (parseSchemaType "response"))
                           , ("CancelRequest", fmap FourOf4 (parseSchemaType "cancel"))
                           ]
    schemaTypeToXML s x@OmiEnvelope{} =
        toXMLElement s [ toXMLAttribute "version" $ omiEnvelope_version x
                       , toXMLAttribute "ttl" $ omiEnvelope_ttl x
                       ]
            [ foldOneOf4  (schemaTypeToXML "read")
                          (schemaTypeToXML "write")
                          (schemaTypeToXML "response")
                          (schemaTypeToXML "cancel")
                          $ omiEnvelope_choice0 x
            ]
 
-- | Root element of Open Messaging Interface.
elementOmiEnvelope :: XMLParser OmiEnvelope
elementOmiEnvelope = parseSchemaType "omiEnvelope"
elementToXMLOmiEnvelope :: OmiEnvelope -> [Content ()]
elementToXMLOmiEnvelope = schemaTypeToXML "omiEnvelope"
 
data Msg = Msg ObjectsType 
        deriving (Eq,Show)
instance SchemaType Msg where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        commit $ interior e $ return Msg
            `apply` head `fmap` between (Occurs (Just 1) (Just 1)) elementObjects
    schemaTypeToXML s (Msg objects) =
        toXMLElement s []
            [ (schemaTypeToXML "msg") objects
            ]
 
elementMsg :: XMLParser Msg
elementMsg = parseSchemaType "msg"
elementToXMLMsg :: Msg -> [Content ()]
elementToXMLMsg = schemaTypeToXML "msg"
 
-- | Base type for "read" and "write" requests.
data RequestBaseType = RequestBaseType
        { requestBaseType_callback :: Maybe Xs.AnyURI
        , requestBaseType_msgformat :: Maybe Xs.XsdString
          -- ^ Text string indicating the format of the payload in "msg", 
          --   e.g. "csv", "obix" or similar. For an XML schema, this 
          --   should correspond to the value of the schema’s “id” 
          --   attribute, if present.
        , requestBaseType_targetType :: Maybe Xs.XsdString
          -- ^ Currently "node" or "device". Using "device" indicates that 
          --   if the message "target object" is some kind of device 
          --   connected to a node, then try to get the requested value 
          --   from the "device".
        , requestBaseType_nodeList :: Maybe NodesType
        , requestBaseType_requestID :: [IdType]
          -- ^ "requestID" is only included when "polling" for data that 
          --   corresponds to an earlier subscription that returned the 
          --   corresponding Id.
        , requestBaseType_msg :: Maybe Msg
        }
        deriving (Eq,Show)
instance SchemaType RequestBaseType where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        a0 <- optional $ getAttribute "callback" e pos
        a1 <- optional $ getAttribute "msgformat" e pos
        a2 <- optional $ getAttribute "targetType" e pos
        commit $ interior e $ return (RequestBaseType a0 a1 a2)
            `apply` optional (parseSchemaType "nodeList")
            `apply` many (parseSchemaType "requestID")
            `apply` optional (elementMsg)
    schemaTypeToXML s x@RequestBaseType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "callback") $ requestBaseType_callback x
                       , maybe [] (toXMLAttribute "msgformat") $ requestBaseType_msgformat x
                       , maybe [] (toXMLAttribute "targetType") $ requestBaseType_targetType x
                       ]
            [ maybe [] (schemaTypeToXML "nodeList") $ requestBaseType_nodeList x
            , concatMap (schemaTypeToXML "requestID") $ requestBaseType_requestID x
            , maybe [] (elementToXMLMsg) $ requestBaseType_msg x
            ]
 
-- | Read request type.
data ReadRequest = ReadRequest
        { readRequest_callback :: Maybe Xs.AnyURI
        , readRequest_msgformat :: Maybe Xs.XsdString
          -- ^ Text string indicating the format of the payload in "msg", 
          --   e.g. "csv", "obix" or similar. For an XML schema, this 
          --   should correspond to the value of the schema’s “id” 
          --   attribute, if present.
        , readRequest_targetType :: Maybe Xs.XsdString
          -- ^ Currently "node" or "device". Using "device" indicates that 
          --   if the message "target object" is some kind of device 
          --   connected to a node, then try to get the requested value 
          --   from the "device".
        , readRequest_interval :: Maybe Xs.XsdString
          -- ^ If an "interval" attibute is included, it indicates that 
          --   this is a subscription request that uses the given 
          --   interval. Values 0, -1 and -2 have special significations.
        , readRequest_oldest :: Maybe Xs.XsdString
          -- ^ Retrieve the oldest available number of historical data 
          --   available.
        , readRequest_begin :: Maybe Xs.DateTime
          -- ^ Retrieve data from this begin date.
        , readRequest_end :: Maybe Xs.DateTime
          -- ^ Retrieve data until this end date.
        , readRequest_newest :: Maybe Xs.XsdString
          -- ^ Retrieve the newest available number of historical data 
          --   available
        , readRequest_nodeList :: Maybe NodesType
        , readRequest_requestID :: [IdType]
          -- ^ "requestID" is only included when "polling" for data that 
          --   corresponds to an earlier subscription that returned the 
          --   corresponding Id.
        , readRequest_msg :: Maybe Msg
        }
        deriving (Eq,Show)
instance SchemaType ReadRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        a0 <- optional $ getAttribute "callback" e pos
        a1 <- optional $ getAttribute "msgformat" e pos
        a2 <- optional $ getAttribute "targetType" e pos
        a3 <- optional $ getAttribute "interval" e pos
        a4 <- optional $ getAttribute "oldest" e pos
        a5 <- optional $ getAttribute "begin" e pos
        a6 <- optional $ getAttribute "end" e pos
        a7 <- optional $ getAttribute "newest" e pos
        commit $ interior e $ return (ReadRequest a0 a1 a2 a3 a4 a5 a6 a7)
            `apply` optional (parseSchemaType "nodeList")
            `apply` many (parseSchemaType "requestID")
            `apply` optional (elementMsg)
    schemaTypeToXML s x@ReadRequest{} =
        toXMLElement s [ maybe [] (toXMLAttribute "callback") $ readRequest_callback x
                       , maybe [] (toXMLAttribute "msgformat") $ readRequest_msgformat x
                       , maybe [] (toXMLAttribute "targetType") $ readRequest_targetType x
                       , maybe [] (toXMLAttribute "interval") $ readRequest_interval x
                       , maybe [] (toXMLAttribute "oldest") $ readRequest_oldest x
                       , maybe [] (toXMLAttribute "begin") $ readRequest_begin x
                       , maybe [] (toXMLAttribute "end") $ readRequest_end x
                       , maybe [] (toXMLAttribute "newest") $ readRequest_newest x
                       ]
            [ maybe [] (schemaTypeToXML "nodeList") $ readRequest_nodeList x
            , concatMap (schemaTypeToXML "requestID") $ readRequest_requestID x
            , maybe [] (elementToXMLMsg) $ readRequest_msg x
            ]
instance Extension ReadRequest RequestBaseType where
    supertype (ReadRequest a0 a1 a2 a3 a4 a5 a6 a7 e0 e1 e2) =
               RequestBaseType a0 a1 a2 e0 e1 e2
 
instance GetOneOf ReadRequest where
    getOneOf = OneOf4

-- | Write request type.
data WriteRequest = WriteRequest
        { writeRequest_callback :: Maybe Xs.AnyURI
        , writeRequest_msgformat :: Maybe Xs.XsdString
          -- ^ Text string indicating the format of the payload in "msg", 
          --   e.g. "csv", "obix" or similar. For an XML schema, this 
          --   should correspond to the value of the schema’s “id” 
          --   attribute, if present.
        , writeRequest_targetType :: Maybe Xs.XsdString
          -- ^ Currently "node" or "device". Using "device" indicates that 
          --   if the message "target object" is some kind of device 
          --   connected to a node, then try to get the requested value 
          --   from the "device".
        , writeRequest_nodeList :: Maybe NodesType
        , writeRequest_requestID :: [IdType]
          -- ^ "requestID" is only included when "polling" for data that 
          --   corresponds to an earlier subscription that returned the 
          --   corresponding Id.
        , writeRequest_msg :: Maybe Msg
        }
        deriving (Eq,Show)
instance SchemaType WriteRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        a0 <- optional $ getAttribute "callback" e pos
        a1 <- optional $ getAttribute "msgformat" e pos
        a2 <- optional $ getAttribute "targetType" e pos
        commit $ interior e $ return (WriteRequest a0 a1 a2)
            `apply` optional (parseSchemaType "nodeList")
            `apply` many (parseSchemaType "requestID")
            `apply` optional (elementMsg)
    schemaTypeToXML s x@WriteRequest{} =
        toXMLElement s [ maybe [] (toXMLAttribute "callback") $ writeRequest_callback x
                       , maybe [] (toXMLAttribute "msgformat") $ writeRequest_msgformat x
                       , maybe [] (toXMLAttribute "targetType") $ writeRequest_targetType x
                       ]
            [ maybe [] (schemaTypeToXML "nodeList") $ writeRequest_nodeList x
            , concatMap (schemaTypeToXML "requestID") $ writeRequest_requestID x
            , maybe [] (elementToXMLMsg) $ writeRequest_msg x
            ]
instance Extension WriteRequest RequestBaseType where
    supertype (WriteRequest a0 a1 a2 e0 e1 e2) =
               RequestBaseType a0 a1 a2 e0 e1 e2
 
instance GetOneOf WriteRequest where
    getOneOf = TwoOf4

-- | List of results.
data ResponseListType = ResponseListType
        { responseListType_result :: [RequestResultType]
        }
        deriving (Eq,Show)
instance SchemaType ResponseListType where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        commit $ interior e $ return ResponseListType
            `apply` many1 (parseSchemaType "result")
    schemaTypeToXML s x@ResponseListType{} =
        toXMLElement s []
            [ concatMap (schemaTypeToXML "result") $ responseListType_result x
            ]

instance GetOneOf ResponseListType where
    getOneOf = ThreeOf4

-- | Result of a request.
data RequestResultType = RequestResultType
        { requestResultType_msgformat :: Maybe Xs.XsdString
          -- ^ Text string indicating the format of the payload in "msg", 
          --   e.g. "csv", "obix" or similar.
        , requestResultType_targetType :: Maybe Xs.XsdString
          -- ^ Currently "node" or "device". Value "device" indicates that 
          --   the response comes directly from the "target object". Value 
          --   "node" is intended to be used for indicating that even 
          --   though the initial request was for "device", the returned 
          --   value is the last known one by this node due to the 
          --   unavailability of the "device" online.
        , requestResultType_return :: [ReturnType]
        , requestResultType_requestID :: Maybe IdType
        , requestResultType_msg :: Maybe Msg
        , requestResultType_nodeList :: Maybe NodesType
        , requestResultType_omiEnvelope :: Maybe OmiEnvelope
          -- ^ Root element of Open Messaging Interface.
        }
        deriving (Eq,Show)
instance SchemaType RequestResultType where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        a0 <- optional $ getAttribute "msgformat" e pos
        a1 <- optional $ getAttribute "targetType" e pos
        commit $ interior e $ return (RequestResultType a0 a1)
            `apply` between (Occurs (Just 1) Nothing)
                            (parseSchemaType "omi:return")
            `apply` optional (parseSchemaType "requestID")
            `apply` optional (elementMsg)
            `apply` optional (parseSchemaType "nodeList")
            `apply` optional (elementOmiEnvelope)
    schemaTypeToXML s x@RequestResultType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "msgformat") $ requestResultType_msgformat x
                       , maybe [] (toXMLAttribute "targetType") $ requestResultType_targetType x
                       ]
            [ concatMap (schemaTypeToXML "return") $ requestResultType_return x
            , maybe [] (schemaTypeToXML "requestID") $ requestResultType_requestID x
            , maybe [] (elementToXMLMsg) $ requestResultType_msg x
            , maybe [] (schemaTypeToXML "nodeList") $ requestResultType_nodeList x
            , maybe [] (elementToXMLOmiEnvelope) $ requestResultType_omiEnvelope x
            ]
 
-- | Return status of request. Use HTTP codes / descriptions 
--   when applicable.
data ReturnType = ReturnType ReturnTypeAttributes deriving (Eq,Show)
data ReturnTypeAttributes = ReturnTypeAttributes
    { returnTypeAttributes_returnCode :: Xs.XsdString
      -- ^ Use HTTP codes when applicable.
    , returnTypeAttributes_description :: Maybe Xs.XsdString
    }
    deriving (Eq,Show)
instance SchemaType ReturnType where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        commit $ do
          a0 <- getAttribute "returnCode" e pos
          a1 <- optional $ getAttribute "description" e pos
          --reparse [CElem e pos]
          --v <- parseSchemaType s
          return $ ReturnType (ReturnTypeAttributes a0 a1)
    schemaTypeToXML s (ReturnType at) =
        addXMLAttributes [ toXMLAttribute "returnCode" $ returnTypeAttributes_returnCode at
                         , maybe [] (toXMLAttribute "description") $ returnTypeAttributes_description at
                         ]
            []
--instance Extension ReturnType Xs.XsdString where
--    supertype (ReturnType s _) = s
 
-- | The nodesType is used anywhere in the schema where lists of 
--   nodes can appear.
data NodesType = NodesType
        { nodesType_type :: Maybe Xs.XsdString
          -- ^ String indicating what format is being used for "node" 
          --   addresses. By default a URL.
        , nodesType_node :: [Xs.AnyURI]
          -- ^ Defines the URL/URI of an O-MI node. The node may be 
          --   located using other means than this URL field, like 
          --   discovery and routing functionality.
        }
        deriving (Eq,Show)
instance SchemaType NodesType where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        a0 <- optional $ getAttribute "type" e pos
        commit $ interior e $ return (NodesType a0)
            `apply` many1 (parseSchemaType "node")
    schemaTypeToXML s x@NodesType{} =
        toXMLElement s [ maybe [] (toXMLAttribute "type") $ nodesType_type x
                       ]
            [ concatMap (schemaTypeToXML "node") $ nodesType_node x
            ]
 
-- | Some kind of identifier with optional "format" attribute 
--   for indicating what kind of identifier is used.
data IdType = IdType Xs.XsdString IdTypeAttributes deriving (Eq,Show)
data IdTypeAttributes = IdTypeAttributes
    { idTypeAttributes_format :: Maybe Xs.XsdString
      -- ^ Use for indicating what kind of identifier.
    }
    deriving (Eq,Show)
instance SchemaType IdType where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        commit $ do
          a0 <- optional $ getAttribute "format" e pos
          reparse [CElem e pos]
          v <- parseSchemaType s
          return $ IdType v (IdTypeAttributes a0)
    schemaTypeToXML s (IdType bt at) =
        addXMLAttributes [ maybe [] (toXMLAttribute "format") $ idTypeAttributes_format at
                         ]
            $ schemaTypeToXML s bt
instance Extension IdType Xs.XsdString where
    supertype (IdType s _) = s
 
-- | Cancel request type.
data CancelRequest = CancelRequest
        { cancelRequest_nodeList :: Maybe NodesType
        , cancelRequest_requestID :: [IdType]
        }
        deriving (Eq,Show)
instance SchemaType CancelRequest where
    parseSchemaType s = do
        (pos,e) <- posnElement' [s]
        commit $ interior e $ return CancelRequest
            `apply` optional (parseSchemaType "nodeList")
            `apply` many1 (parseSchemaType "requestID")
    schemaTypeToXML s x@CancelRequest{} =
        toXMLElement s []
            [ maybe [] (schemaTypeToXML "nodeList") $ cancelRequest_nodeList x
            , concatMap (schemaTypeToXML "requestID") $ cancelRequest_requestID x
            ]


instance GetOneOf CancelRequest where
    getOneOf = FourOf4


