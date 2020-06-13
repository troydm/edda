{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module EDDA.Data.Database where

import EDDA.Types
import EDDA.Data.Document (toDocument, fromDocument, valStr)

import Control.Monad (void,join)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Exception (evaluate)

import Data.Maybe (maybe)
import Database.MongoDB
import Database.MongoDB.Query
import Data.Int (Int32(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

query action = do
                conf <- ask
                pipe <- liftIO $ connect (readHostPort (mongoHost conf))
                result <- liftIO $ access pipe master (mongoDb conf) action
                liftIO $ close pipe
                return result


systemDoc edsmId systemName = ["edsmId" := val edsmId, "systemName" := valStr systemName]
stationDoc systemName stationName = ["systemName" := valStr systemName, "stationName" := valStr stationName]
stationEddbIdDoc eddbId = ["eddbId" := val eddbId]

nextFoldl :: (Document -> Maybe a) -> (b -> a -> b) -> b -> Cursor -> Action IO (Maybe b)
nextFoldl f g i c = rec i
              where rec !acc = next c >>= \r -> maybe (return (Just acc)) (maybe (return Nothing) (\a -> let !macc = g acc a in rec macc) . f) r

type SystemPair = (Int32,T.Text)
type EddbIdMap = HM.HashMap Int32 T.Text

getSystemEDDBIdsAction = find (select [] "systems") { project = ["eddbId" =: Int32 1, "systemName" =:  Int32 1, "_id" =: Int32 0] } >>= nextFoldl docToPair mergeToHashMap HM.empty
    where docToPair :: Document -> Maybe SystemPair
          docToPair doc = do !eddbId <- Database.MongoDB.lookup "eddbId" doc :: Maybe Int
                             !systemName <- Database.MongoDB.lookup "systemName" doc :: Maybe T.Text
                             return (fromIntegral eddbId, systemName)
          mergeToHashMap :: EddbIdMap -> SystemPair -> EddbIdMap
          mergeToHashMap !acc (!k,!v) = HM.insert k v acc
    
getSystemsByNameCursor systemNames = find (select ["$or" =: Array (map (\s -> Doc ["systemName" =: valStr s]) systemNames)] "systems") >>= rest
getSystemsByEddbIdsCursor eddbIds = find (select ["$or" =: Array (map (\i -> Doc ["eddbId" =: val i]) eddbIds)] "systems") >>= rest
getSystemsByEdsmIdsCursor edsmIds = find (select ["$or" =: Array (map (\i -> Doc ["edsmId" =: val i]) edsmIds)] "systems") >>= rest

getStationsBySystemNameCursor systemNames = find (select ["$or" =: Array (map (\s -> Doc ["systemName" =: valStr s]) systemNames)] "stations") >>= rest
getStationsByEddbIdsCursor eddbIds = find (select ["$or" =: Array (map (\i -> Doc ["eddbId" =: val i]) eddbIds)] "stations") >>= rest

getSystemCoordAction systemName = findOne (select ["systemName" =: valStr systemName] "systems") { project = ["edsmId" =: Int32 1, "systemName" =:  Int32 1, "x" =: Int32 1, "y" =: Int32 1, "z" =: Int32 1, "_id" =: Int32 0] }
getSystemCoordsCursor = find (select [] "systems") { project = ["edsmId" =: Int32 1, "systemName" =:  Int32 1, "x" =: Int32 1, "y" =: Int32 1, "z" =: Int32 1, "_id" =: Int32 0] }

getSystemEDDBIdsMap :: ConfigT (Maybe EddbIdMap)
getSystemEDDBIdsMap = do !systems <- query getSystemEDDBIdsAction
                         return systems

getSystemCoord :: Str -> ConfigT (Maybe SystemCoord)
getSystemCoord systemName = do systemCoord <- query (getSystemCoordAction systemName)
                               return $ (fromDocument . Doc) =<< systemCoord
                                

getSystemCoordsLoop :: Cursor -> (SystemCoord -> Bool) -> [SystemCoord] -> Action IO (Maybe [SystemCoord])
getSystemCoordsLoop c f acc = 
                              do !d <- next c
                                 case d of
                                    Just doc -> case fromDocument (Doc doc) of
                                                    Just !coord -> if f coord 
                                                                   then getSystemCoordsLoop c f (coord : acc) 
                                                                   else getSystemCoordsLoop c f acc
                                                    Nothing -> return Nothing
                                    Nothing -> return (Just acc)

getSystemCoords :: (SystemCoord -> Bool) -> ConfigT (Maybe [SystemCoord])
getSystemCoords f = query (getSystemCoordsCursor >>= \c -> getSystemCoordsLoop c f [])

getAllSystemCoords :: ConfigT (V.Vector SystemCoord)
getAllSystemCoords = do coords <- query (getSystemCoordsCursor >>= rest)
                        return $ maybe V.empty V.fromList (allJust (map (fromDocument . Doc) coords :: [Maybe SystemCoord]))

saveSystems :: [(Int32,Str,Document)] -> Action IO ()
saveSystems systems =
    void $ updateAll "systems" systemsMapped
    where systemsMapped = map (\(i,n,d) -> (systemDoc i n, ["$set" := Doc d], [Upsert])) systems

saveStations :: [(Str,Str,Document)] -> Action IO ()
saveStations stations =
    void $ updateAll "stations" stationsMapped
    where stationsMapped = map (\(s,n,d) -> (stationDoc s n, ["$set" := Doc d], [Upsert])) stations

saveStationsCommodities :: HM.HashMap Int32 (V.Vector Document) -> Action IO ()
saveStationsCommodities stations =
    void $ updateAll "stations" stationsMapped
    where stationsMapped = map (\(sId,d) -> (stationEddbIdDoc sId, ["$set" := Doc ["commodities" =: Array (V.toList (V.map Doc d))]], [])) (HM.toList stations)

saveStationInfo :: Str -> Str -> Document -> ConfigT ()
saveStationInfo systemName stationName info =
    query $ upsert (select (stationDoc systemName stationName) "stations") info

saveShips :: Str -> Str -> Ships -> ConfigT ()
saveShips systemName stationName ships = saveStationInfo systemName stationName ["$set" := toDocument ships ]

saveCommodities :: Str -> Str -> Commodities -> ConfigT ()
saveCommodities systemName stationName commodities = saveStationInfo systemName stationName ["$set" := toDocument commodities]

saveOutfittings :: Str -> Str -> Outfittings -> ConfigT ()
saveOutfittings systemName stationName outfittings = saveStationInfo systemName stationName ["$set" := toDocument outfittings]

saveError :: Str -> Str -> ConfigT ()
saveError errorType msg = query $ insert_ "errors" ["type" := valStr errorType, "message" := valStr msg]
