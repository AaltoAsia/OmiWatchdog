module Main where

--import Text.Read (readMaybe)
--import Data.Maybe (listToMaybe)
import Control.Monad (when)

--import qualified Network.HTTP.Server as HTTP
--import Network.HTTP.Server (Config (..))
--import Network.HTTP.Server.Logger (stdLogger)
--import Network.Socket (PortNumber(..))

import System.Environment (getArgs)
import System.IO (hPutStr, stderr)

import OmiClient
import Omi
import Odf


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

logError :: String -> IO ()
logError = hPutStr stderr


processData :: PathValues -> IO ()
processData = undefined

main :: IO ()
main = do
    args <- getArgs
    --let configM :: Maybe [(String, Double)]
    --    configM = listToMaybe args >>= readMaybe

    --case configM of
    --    Nothing -> print usage
    --    Just config -> undefined


    result <- sendRequest (head args) readAllRequest
    case result of
        Right response -> do
            let (omiReturnCode, desc) = head $ getReturnStatus response
            when (omiReturnCode < 200 && omiReturnCode >= 300) $
                logError $ "O-MI error code " ++ show omiReturnCode ++ ":" ++ desc

            let objects = getObjects response
                pathValues = flattenOdf objects

            processData pathValues

        Left err -> logError err
    
    


