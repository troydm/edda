{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module EDDA.Data.Database where

import EDDA.Types
import EDDA.Data.Document (toDocument, fromDocument, valStr)

import Control.Monad (join)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Exception (evaluate)

import Database.MongoDB
import Database.MongoDB.Query
import Data.Int (Int32(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

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
              where rec !acc = next c >>= \r -> maybe (return (Just acc)) (\d -> maybe (return Nothing) (\a -> let !macc = g acc a in rec macc) (f d)) r

type SystemPair = (Int32,T.Text)
type EddbIdMap = HM.HashMap Int32 T.Text

getSystemEDDBIdsAction = find (select [] "systems") { project = ["eddbId" =: Int32 1, "systemName" =:  Int32 1, "_id" =: Int32 0] } >>= nextFoldl docToPair mergeToHashMap HM.empty
    where docToPair :: Document -> Maybe SystemPair
          docToPair doc = do !eddbId <- Database.MongoDB.lookup "eddbId" doc :: Maybe Int
                             !systemName <- Database.MongoDB.lookup "systemName" doc :: Maybe T.Text
                             return (fromIntegral eddbId, systemName)
          mergeToHashMap :: EddbIdMap -> SystemPair -> EddbIdMap
          mergeToHashMap !acc (!k,!v) = HM.insert k v acc
    
    
    
getSystemCoordAction systemName = findOne (select ["systemName" =: valStr systemName] "systems") { project = ["edsmId" =: Int32 1, "systemName" =:  Int32 1, "x" =: Int32 1, "y" =: Int32 1, "z" =: Int32 1, "_id" =: Int32 0] }
getSystemCoordsCursor = find (select [] "systems") { project = ["edsmId" =: Int32 1, "systemName" =:  Int32 1, "x" =: Int32 1, "y" =: Int32 1, "z" =: Int32 1, "_id" =: Int32 0] }

getSystemEDDBIdsMap :: ConfigT (Maybe EddbIdMap)
getSystemEDDBIdsMap = do !systems <- query getSystemEDDBIdsAction
                         return systems

getSystemCoord :: Str -> ConfigT (Maybe SystemCoord)
getSystemCoord systemName = do systemCoord <- query (getSystemCoordAction systemName)
                               return $ join ((\d -> fromDocument (Doc d)) <$> systemCoord)
                                

getSystemCoordsLoop :: Cursor -> (SystemCoord -> Bool) -> [SystemCoord] -> Action IO (Maybe [SystemCoord])
getSystemCoordsLoop c f acc = 
                              do d <- next c
                                 case d of
                                    Just doc -> case fromDocument (Doc doc) of
                                                    Just coord -> if f coord 
                                                                  then getSystemCoordsLoop c f (coord : acc) 
                                                                  else getSystemCoordsLoop c f acc
                                                    Nothing -> return Nothing
                                    Nothing -> return (Just acc)

getSystemCoords :: (SystemCoord -> Bool) -> ConfigT (Maybe [SystemCoord])
getSystemCoords f = query (getSystemCoordsCursor >>= \c -> getSystemCoordsLoop c f [])

saveSystems :: [(Int32,Str,Document)] -> ConfigT ()
saveSystems systems = 
    query $ updateAll "systems" systemsMapped >> return ()
    where systemsMapped = map (\(i,n,d) -> ((systemDoc i n), ["$set" := Doc d], [Upsert])) systems

saveStations :: [(Str,Str,Document)] -> ConfigT ()
saveStations stations = 
    query $ updateAll "stations" stationsMapped >> return ()
    where stationsMapped = map (\(s,n,d) -> ((stationDoc s n), ["$set" := Doc d], [Upsert])) stations

saveStationsCommodities :: HM.HashMap Int32 [Document] -> ConfigT ()
saveStationsCommodities stations = 
    query $ updateAll "stations" stationsMapped >> return ()
    where stationsMapped = map (\(sId,d) -> ((stationEddbIdDoc sId), ["$set" := Doc ["commodities" =: (Array (map Doc d))]], [])) (HM.toList stations)

saveStationInfo :: Str -> Str -> Document -> ConfigT ()
saveStationInfo systemName stationName info = 
    query $ upsert (select (stationDoc systemName stationName) "stations") info

saveShips :: Str -> Str -> Ships -> ConfigT ()
saveShips systemName stationName ships = saveStationInfo systemName stationName $ ["$set" := toDocument ships ]

saveCommodities :: Str -> Str -> Commodities -> ConfigT ()
saveCommodities systemName stationName commodities = saveStationInfo systemName stationName $ ["$set" := toDocument commodities]

saveOutfittings :: Str -> Str -> Outfittings -> ConfigT ()
saveOutfittings systemName stationName outfittings = saveStationInfo systemName stationName $ ["$set" := toDocument outfittings]

saveError :: Str -> Str -> ConfigT ()
saveError errorType msg = query $ insert_ "errors" ["type" := valStr errorType, "message" := valStr msg]
