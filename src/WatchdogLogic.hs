{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module WatchdogLogic where

import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (<|))
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, NominalDiffTime)

import Odf

-- calculate running avarage and standard deviation for update interval
-- save to acid-state
-- go through unchanged:
--  To filter out false positives:
--       longerThanUsual =
--          if std.deviation < 60s && avarage*num > 1h
--          then
--              3 * avarage
--          else
--              3 * std.deviation
--       
--  alert if time > (avarage + longerThanUsual)

numberOfHistory :: Int
numberOfHistory = 200

data DelayStore = DelayStore {allData :: !(Map Path StatisticsModel)}

data StatisticsModel = StatisticsModel
    { sm_lastSeen :: !UTCTime
    , sm_alertTriggerTime :: !UTCTime
    , sm_delayAverage :: !Double
    , sm_delayStdDeviation :: !Double
    , sm_intervalData :: !(Seq NominalDiffTime)
    , sm_isNew :: Bool
    } deriving (Typeable, Show)

emptyDelayStore :: DelayStore
emptyDelayStore = DelayStore Map.empty

getAllData :: Query DelayStore (Map Path StatisticsModel)
getAllData = allData <$> ask

processData :: PathValues -> Update DelayStore ()
processData = mapM_ processPath

processPath :: PathValue -> Update DelayStore ()
processPath (path, (OdfValue newTime _ _)) = do
    (DelayStore store) <- get
    let newTempData = 
            StatisticsModel { sm_lastSeen = newTime
                            , sm_alertTriggerTime = (24 * 60 * 60) `addUTCTime` newTime
                            , sm_delayAverage = 0.0
                            , sm_delayStdDeviation = 0.0
                            , sm_intervalData = Seq.empty
                            , sm_isNew = True
                            }
    put $ DelayStore $
        Map.insertWith updateStatistics path newTempData store

updateStatistics :: StatisticsModel -> StatisticsModel -> StatisticsModel
updateStatistics new old = old { sm_lastSeen          = newTime
                               , sm_alertTriggerTime  = longerThanUsual `addUTCTime` newTime
                               , sm_delayAverage      = average
                               , sm_delayStdDeviation = stdDeviation
                               , sm_intervalData      = newSeq
                               , sm_isNew = False
                               }
  where
    StatisticsModel{sm_lastSeen=newTime} = new 
    StatisticsModel{sm_lastSeen=oldTime, sm_intervalData=oldSeq} = old 
    --checkAlert path StatisticsModel{sm_alertTriggerTime=alertTime, sm_intervalData=dataSeq, sm_isNew=isNew} = 
    toDouble :: NominalDiffTime -> Double
    toDouble = realToFrac
    toNominal :: Double -> NominalDiffTime
    toNominal = realToFrac

    addedVals = 1
    numVals = Seq.length oldSeq
    newSeq' = (newTime `diffUTCTime` oldTime) <| oldSeq
    newSeq  = Seq.take numberOfHistory newSeq'
    num :: Double
    num = realToFrac $ numVals + addedVals
    divisor :: Double
    divisor = 1.0 / num
    average :: Double
    average = realToFrac $ divisor * (toDouble $ sum newSeq)

    stdDeviation = sqrt $
        divisor * sum ((\x -> (toDouble x - average) ^^ (2::Int)) <$> newSeq)
    longerThanUsual :: NominalDiffTime
    longerThanUsual = toNominal $ average +
       if stdDeviation < 60.0 &&
           average*num > 1.0*60*60 -- one hour
       then
           3 * average
       else
           3 * stdDeviation

{- TODO
median :: (Fractional a, Num a) => Seq a -> a
median x | null x = 
         | odd n  = case Seq.head $ Seq.drop oddI x' of
                        Nothing -> 0  -- Not a number?
         | even n = mean $ take 2 $ drop evenI x'
         where
             x'    = Seq.unstableSort x
             evenI = oddI - 1
             oddI  = (n `div` 2)
             n     = length x
             mean [a, b] = (a + b) / 2
-}


type Alert = (AlertType, Path)
data AlertType = Warning
               | Missing
               | Appeared
               deriving (Eq, Show, Read, Typeable)



-- | Check for alerts with the given time as current time.
checkAlerts :: UTCTime -> Update DelayStore [Alert]
checkAlerts currentTime = do
    dataMap <- allData <$> get
    let events = concatMap checkAlert $ Map.toList dataMap
        newItems = map snd $ filter isAppearedEvent events

    -- remove isNew flags:
    put $ DelayStore $ removeNewFlags dataMap newItems
    return events

  where
    removeNewFlags dataMap (path : rest) =
       let removeFlag model = model {sm_isNew = False}
           newMap = Map.adjust removeFlag path dataMap
       in removeNewFlags newMap rest
    removeNewFlags dataMap [] = dataMap

    isAppearedEvent (Appeared, _) = True
    isAppearedEvent _ = False

    checkAlert :: (Path, StatisticsModel) -> [Alert]
    checkAlert (path, StatisticsModel{sm_alertTriggerTime=alertTime, sm_intervalData=dataSeq, sm_isNew=isNew}) = 
        let num = Seq.length dataSeq
            oneDay = realToFrac (24*60*60 :: Int)
        in if num > 3 then
            if isNew then [(Appeared, path)]
            else if currentTime > alertTime then
                if currentTime > addUTCTime oneDay alertTime
                then [(Missing, path)]
                else [(Warning, path)]
            else []
        else []


-- * TH Deriving stuff
-- | Make instances etc for acid

-- deriveSafeCopy version 'base|'extension ''Type
deriveSafeCopy 1 'base ''OdfValue
deriveSafeCopy 1 'base ''Path
deriveSafeCopy 1 'base ''DelayStore
deriveSafeCopy 1 'base ''StatisticsModel
deriveSafeCopy 1 'base ''AlertType

$(makeAcidic ''DelayStore ['processData, 'processPath, 'getAllData, 'checkAlerts])

