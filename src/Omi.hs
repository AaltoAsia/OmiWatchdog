{-# LANGUAGE OverloadedStrings #-}
module Omi where


import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Text.XML.HaXml.Schema.PrimitiveTypes (XsdString(..))
import Text.XML.HaXml.OneOfN

import OmiXsd
import OdfXsd
import Util.HaXml()

-- * Utilities for request generation

mkReadRequest :: ReadRequest
mkReadRequest = ReadRequest 
        { readRequest_callback = Nothing :: Maybe Xs.AnyURI
        , readRequest_msgformat = Just "odf" :: Maybe Xs.XsdString
          -- ^ Text string indicating the format of the payload in "msg", 
          --   e.g. "csv", "obix" or similar. For an XML schema, this 
          --   should correspond to the value of the schema’s “id” 
          --   attribute, if present.
        , readRequest_targetType = Nothing :: Maybe Xs.XsdString
          -- ^ Currently "node" or "device". Using "device" indicates that 
          --   if the message "target object" is some kind of device 
          --   connected to a node, then try to get the requested value 
          --   from the "device".
        , readRequest_interval = Nothing :: Maybe Xs.XsdString
          -- ^ If an "interval" attibute is included, it indicates that 
          --   this is a subscription request that uses the given 
          --   interval. Values 0, -1 and -2 have special significations.
        , readRequest_oldest = Nothing :: Maybe Xs.XsdString
          -- ^ Retrieve the oldest available number of historical data 
          --   available.
        , readRequest_begin = Nothing :: Maybe Xs.DateTime
          -- ^ Retrieve data from this begin date.
        , readRequest_end = Nothing :: Maybe Xs.DateTime
          -- ^ Retrieve data until this end date.
        , readRequest_newest = Nothing :: Maybe Xs.XsdString
          -- ^ Retrieve the newest available number of historical data 
          --   available
        , readRequest_nodeList = Nothing :: Maybe NodesType
        , readRequest_requestID = [] :: [IdType]
          -- ^ "requestID" is only included when "polling" for data that 
          --   corresponds to an earlier subscription that returned the 
          --   corresponding Id.
        , readRequest_msg = Nothing :: Maybe Msg
        }

-- TODO: use types instead
readAllRequest :: String
readAllRequest = "\
    \<?xml version=\"1.0\"?>\
    \<omi:omiEnvelope xmlns:omi=\"omi.xsd\" version=\"1.0\" ttl=\"10\">\
        \<omi:read msgformat=\"odf\">\
            \<omi:msg>\
                \<Objects xmlns=\"odf.xsd\" />\
            \</omi:msg>\
        \</omi:read>\
    \</omi:omiEnvelope>"

-- TODO
--mkReadAllRequest = mkReadRequest {
--    readRequest_msg = Just undefined
--  }


-- * Utilities for parse results

-- TODO: Joint the next two functions and make a new type (OmiResponse?)
getObjects :: OmiEnvelope -> ObjectsType
getObjects (OmiEnvelope _ _ (ThreeOf4 (ResponseListType responses))) = objectsFromResponse $ head responses
  where
    objectsFromResponse (RequestResultType _ _ _ _ (Just (Msg objs)) _ _) = objs
    objectsFromResponse _ = undefined
getObjects _ = undefined

getReturnStatus :: OmiEnvelope -> [(Int, String)]
getReturnStatus (OmiEnvelope _ _ (ThreeOf4 (ResponseListType responses))) =

    -- FIXME? concatMap: Loses information about multiresponse vs multireturn
    concatMap responseReturnCodes responses

  where 
    responseReturnCodes (RequestResultType _ _ multipleReturn _ _ _ _) =
        map returnReturnCodes multipleReturn
    returnReturnCodes (ReturnType (ReturnTypeAttributes (XsdString code) mDescription)) =
        (read code, maybe "" (\(XsdString a) -> a) mDescription)

getReturnStatus _ = []
