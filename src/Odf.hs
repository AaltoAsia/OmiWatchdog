{-# LANGUAGE OverloadedStrings, FlexibleInstances, TypeSynonymInstances #-}
module Odf where

import Data.Maybe
import Data.Int
import qualified Text.XML.HaXml.Schema.PrimitiveTypes as Xs
import Text.XML.HaXml.Schema.PrimitiveTypes (XsdString, SimpleType(..))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.ISO8601 (parseISO8601)

import OdfXsd
import Util.HaXml()

newtype Path = Path {fromPath :: String} deriving(Show, Read, Eq, Ord)

class PathLike p where
    toPath :: p -> Path
    toPath = Path . asString
    asString :: p -> String
instance PathLike XsdString where
    asString = simpleTypeText
instance PathLike String where
    asString = id
instance PathLike Path where
    toPath = id
    asString = fromPath

join :: PathLike p => Path -> p -> Path
join a b = toPath (asString a ++ "/" ++ asString b)

pathObjects :: Path
pathObjects = Path "Objects"

data OdfValue = OdfValue 
    { odfValue_timestamp :: UTCTime
    , odfValue_type :: Maybe String
    , odfValue_value :: String
    }
fromValueType :: ValueType -> OdfValue
fromValueType (
    ValueType val (ValueTypeAttributes valType mDateTime mUnixSeconds )) =

        -- Prioritize datetime because it currently has better accuracy
        let mTime = case (mDateTime, mUnixSeconds) of
                (Just dateTime, _)      -> parseISO8601 $ simpleTypeText dateTime
                (Nothing, Just seconds) ->
                    let unwrapLong :: Xs.Long -> Int64
                        unwrapLong (Xs.Long secs) = secs
                    in Just . posixSecondsToUTCTime . realToFrac . unwrapLong $ seconds
                (Nothing, Nothing)      -> Nothing

            -- TODO: pass current time to this function
            defaultTime = error "O-DF value did not have time or it was invalid"

            time = fromMaybe defaultTime mTime

        in OdfValue time (simpleTypeText `fmap` valType) (simpleTypeText val)


mkObjects :: ObjectsType
mkObjects = ObjectsType
        { objectsType_version = Just "1.0" :: Maybe XsdString
          -- ^ Schema version used.
        , objectsType_object = [] :: [ObjectType]
        }
 
type PathValue = (Path, OdfValue)
type PathValues = [PathValue]

-- | flatten ObjectsType to list of Path to value
flattenOdf :: ObjectsType -> PathValues
flattenOdf (ObjectsType _ objs) = concatMap (flattenObject pathObjects) objs
  where
    flattenObject :: Path -> ObjectType -> PathValues
    flattenObject parent (ObjectType _ ids _ infos objects) =
        let [QlmID name _] = ids -- Should be non-empty-list
            path = parent `join` name
        in
        concatMap (pathValue path) infos ++
            concatMap (flattenObject path) objects

    pathValue :: Path -> InfoItemType -> PathValues
    pathValue parent (InfoItemType name _ _ _ values) =
        map (\v -> (toPath $ parent `join` name, fromValueType v)) values


