{-# LANGUAGE OverloadedStrings #-}
module Omi where

import Text.XML.HaXml.Schema.PrimitiveTypes (XsdString)

import OmiXsd


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

mkReadAllRequest = mkReadRequest {
    readRequest_msg = Just undefined
}
