module OmiClient where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import OmiXsd

sendRequest :: OmiEnvelope -> IO (OmiEnvelope)
sendRequest request = undefined



