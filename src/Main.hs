module Main where

--import qualified Network.HTTP.Server as HTTP
--import Network.HTTP.Server (Config (..))
--import Network.HTTP.Server.Logger (stdLogger)
--import Network.Socket (PortNumber(..))

import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (listToMaybe)

import OdfXsd
import OmiXsd
import Util.HaXml
import OmiClient
--import OmiServiceHandler

{- TODO: Server needed when doing a subscription, currently using "read Objects"
serverConfig :: HTTP.Config
serverConfig = HTTP.Config
    { srvLog  = stdLogger 
    , srvHost = "127.0.0.1"
    , srvPort = PortNum 8010
    }

createServer :: IO ()
createServer = HTTP.serverWith serverConfig omiHandler
-}

usage :: String
usage = "Takes a list of tuples as parameter, where the first"++
    " element is the path and the second is its maximum update"++
    " time in minutes"

main :: IO ()
main = do
    args <- getArgs
    let configM :: Maybe [(String, Double)]
        configM = listToMaybe args >>= readMaybe

    case configM of
        Nothing -> print usage
        Just config -> undefined

    
    


