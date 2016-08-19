module OmiClient where

import Data.String
import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Dispatch.Core

import OmiXsd
import Util.HaXml
--import Omi

-- TODO:
-- sendRequest :: String -> OmiEnvelope -> 
sendRequest :: String -> String -> IO (Either String OmiEnvelope)
sendRequest url request =
    -- For HTTP package

    --let requestBody = request
    --    omiRequest = postRequestWithBody url "text/xml" requestBody
    --in do
    --    httpResponse   <- simpleHTTP omiRequest
    --    httpReturnCode <- getResponseCode httpResponse
    --    responseString <- getResponseBody httpResponse

    -- For http-dispatch package
    do
        (HTTPResponse status _ responseString) <- runRequest $ post url (fromString request)
        let eitherStatus = if status /= 200 then Left $ "HTTP Error: " ++ show status else Right ()
            result = do
                eitherStatus
                parseText' url elementOmiEnvelope $ unpack responseString

        return result 



