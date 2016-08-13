module OmiServiceHandler where

-- TODO: Currently not used

{-
import qualified Network.HTTP.Server as HTTP
import qualified Network.HTTP.Server.Response as Response
import qualified Network.HTTP.Headers as Headers
import qualified Data.Text as Text
import Data.Text (Text)
import Text.XML.HaXml.Schema.Schema (SchemaType(..))
import Text.XML.HaXml.Schema.PrimitiveTypes (XsdString)
import Util.HaXml (parseText)
import OmiXsd

omiHandler :: HTTP.Handler String
omiHandler _ _ (HTTP.Request _ HTTP.POST _ request) = do
    return $ handleOmiMessage request
    undefined
    -- TODO: Get parsed data and do something for data 
    -- save to ACID state
omiHandler _ _ _ = return $ Response.respond Response.NotFound

parseOmiMessage :: String -> HTTP.Response String
parseOmiMessage request =
    let parseResult = parseText elementOmiEnvelope request
    in case parseResult of
        Left error        -> omiStatusResponse 400 $ Just $ "Invalid O-MI message: " ++ error
        Right omiEnvelope -> omiOkResponse

omiResponse :: OmiEnvelope -> HTTP.Response String
omiResponse t = 
    let xml = schemaTypeToXML "<wtf>" t
        httpResponse = Response.respond Response.OK
        xmlResponse = httpResponse
            { HTTP.rspBody = show xml
            , HTTP.rspHeaders = [Headers.mkHeader HTTP.HdrContentType "text/xml"]
            }
    in xmlResponse

-- | Int is returnCode, Maybe String is description for the return element
omiStatusResponse :: Int -> Maybe String -> HTTP.Response String
omiStatusResponse = omiResponse _xmltypes

omiOkResponse :: HTTP.Response String
omiOkResponse = omiStatusResponse 200 Nothing
-}
