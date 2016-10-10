module Main where

--import Text.Read (readMaybe)
--import Data.Maybe (listToMaybe)
import Control.Monad (when, forM_)
import System.Environment (getArgs)
import System.IO (hPutStr, stderr)
import Data.Time.Clock (getCurrentTime)

--import qualified Network.HTTP.Server as HTTP
--import Network.HTTP.Server (Config (..))
--import Network.HTTP.Server.Logger (stdLogger)
--import Network.Socket (PortNumber(..))
import Data.Acid

import WatchdogLogic
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
usage = "give url plz"

logError :: String -> IO ()
logError = hPutStr stderr



main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn usage
    else
        runProgram args

runProgram :: [String] -> IO ()
runProgram args = do
    --let configM :: Maybe [(String, Double)]
    --    configM = listToMaybe args >>= readMaybe

    --case configM of
    --    Nothing -> print usage
    --    Just config -> undefined

    delayValues <- openLocalState emptyDelayStore

    result <- sendRequest (head args) readAllRequest
    case result of
        Right response -> do
            let (omiReturnCode, desc) = head $ getReturnStatus response
            when (omiReturnCode < 200 && omiReturnCode >= 300) $
                logError $ "O-MI error code " ++ show omiReturnCode ++ ":" ++ desc

            let objects = getObjects response
                pathValues = flattenOdf objects

            update delayValues $ ProcessData pathValues

            currentTime <- getCurrentTime
            alerts <- update delayValues $ CheckAlerts currentTime
            forM_ alerts $ \a -> putStrLn $ (show $ fst a) ++ " " ++ (show $ snd a)
            return ()

        Left err -> logError err
    
    


