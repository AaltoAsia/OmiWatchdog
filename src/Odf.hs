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

newtype Path = Path String

class ToPath p where
    toPath :: p -> Path
instance ToPath XsdString where
    toPath = Path . simpleTypeText
instance ToPath String where
    toPath = Path


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

flattenOdf :: ObjectsType -> PathValues
flattenOdf (ObjectsType _ objs) = concatMap flattenObject objs
  where
    flattenObject :: ObjectType -> PathValues
    flattenObject (ObjectType _ _ _ infos objects) =
        concatMap pathValue infos ++
            concatMap flattenObject objects

    pathValue :: InfoItemType -> PathValues
    pathValue (InfoItemType name _ _ _ values) =
        map (\v -> (toPath name, fromValueType v)) values

