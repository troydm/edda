{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Rest where

import Web.Scotty
import Network.Wai.Middleware.Gzip

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Log.Logger
import Data.Int
import Data.Bson (Value(Doc))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Attoparsec.Text as P

import EDDA.Types
import EDDA.Data.Database (getAllSystemCoords)
import EDDA.Data.Document (toAeson)
import EDDA.Data.Cache
import EDDA.Data.Query (getSystemsWithinLyFrom',getSystemsByNames,getSystemsByEddbIds,getSystemsByEdsmIds,getStationsBySystemNames,getStationsByEddbIds)


names :: Str -> [Str]
names t =
        case P.parseOnly nameList t of
            Right s -> s
            Left _ -> []
        where nameList :: P.Parser [Str]
              nameList = map T.pack <$> (P.many1' (P.notChar ',')) `P.sepBy'` (P.char ',' >> (P.many' P.space))

ids :: Str -> [Int32]
ids t =
        case P.parseOnly idList t of
            Right s -> s
            Left _ -> []
        where idList :: P.Parser [Int32]
              idList = P.decimal `P.sepBy'` (P.char ',' >> (P.many' P.space))

jsonHeader = setHeader "content-type" "text/json"

jsonOut val = jsonHeader >> raw (encodePretty val)

servicePath path s = capture $ path ++ s

systemsByName c path = get (servicePath path "/systemsByName/:systemNames") $ do
                            systemNames <- param "systemNames"
                            let systemNamesList = names systemNames
                            systems <- liftIO (runReaderT (getSystemsByNames systemNamesList) c)
                            jsonOut (map (toAeson . Doc) systems)

systemsByEddbId c path = get (servicePath path "/systemsByEddbId/:eddbIds") $ do
                            eddbIds <- param "eddbIds"
                            let eddbIdsList = ids eddbIds
                            systems <- liftIO (runReaderT (getSystemsByEddbIds eddbIdsList) c)
                            jsonOut (map (toAeson . Doc) systems)

systemsByEdsmId c path = get (servicePath path "/systemsByEdsmId/:edsmIds") $ do
                            edsmIds <- param "edsmIds"
                            let edsmIdsList = ids edsmIds
                            systems <- liftIO (runReaderT (getSystemsByEdsmIds edsmIdsList) c)
                            jsonOut (map (toAeson . Doc) systems)

stationsBySystemName c path = get (servicePath path "/stationsBySystemName/:systemNames") $ do
                            systemNames <- param "systemNames"
                            let systemNamesList = names systemNames
                            stations <- liftIO (runReaderT (getStationsBySystemNames systemNamesList) c)
                            jsonOut (map (toAeson . Doc) stations)

stationsByEddbId c path = get (servicePath path "/stationsByEddbId/:eddbIds") $ do
                            eddbIds <- param "eddbIds"
                            let eddbIdsList = ids eddbIds
                            stations <- liftIO (runReaderT (getStationsByEddbIds eddbIdsList) c)
                            jsonOut (map (toAeson . Doc) stations)

systemsWithinLy cache c path = get (servicePath path "/systemsWithinLy/:systemName/:distance") $ do
                            allSystemCoords <- liftIO (cachedForkIO cache (runReaderT getAllSystemCoords c))
                            systemName <- param "systemName"
                            distance <- param "distance" :: ActionM Double
                            maybeSystems <- liftIO (runReaderT (getSystemsWithinLyFrom' systemName distance allSystemCoords) c)
                            let systemNamesList = maybe ([] :: [T.Text]) id maybeSystems
                            systems <- liftIO (runReaderT (getSystemsByNames systemNamesList) c)
                            jsonOut (map (toAeson . Doc) systems)

stationsWithinLy cache c path = get (servicePath path "/stationsWithinLy/:systemName/:distance") $ do
                            allSystemCoords <- liftIO (cachedForkIO cache (runReaderT getAllSystemCoords c))
                            systemName <- param "systemName"
                            distance <- param "distance" :: ActionM Double
                            maybeSystems <- liftIO (runReaderT (getSystemsWithinLyFrom' systemName distance allSystemCoords) c)
                            let systemNamesList = maybe ([] :: [T.Text]) id maybeSystems
                            stations <- liftIO (runReaderT (getStationsBySystemNames systemNamesList) c)
                            jsonOut (map (toAeson . Doc) stations)
                            

startService :: Config -> IO ()
startService c = do 
                   let port = (restPort c)
                   let path = (restPath c)
                   let tpath = TL.pack path
                   allSystemsCache <- newCache (restCacheTimeout c)
                   infoM "EDDA.Rest" ("Rest service started on port: " ++ (show port))
                   scotty port $ do
                                  middleware $ gzip def
                                  systemsByName c path
                                  systemsByEddbId c path
                                  systemsByEdsmId c path
                                  stationsBySystemName c path
                                  stationsByEddbId c path
                                  systemsWithinLy allSystemsCache c path
                                  stationsWithinLy allSystemsCache c path
                                  get (servicePath path "") $ do
                                                        html $ mconcat ["<html><title>EDDA REST API</title><body><h2>EDDA REST API</h2><br/><br/>",
                                                                        "<a href=\"",tpath,"/systemsByName/:systemNames\">",tpath,"/systemsByName/:systemNames</a><br/><br/>",
                                                                        "<a href=\"",tpath,"/systemsByEddbId/:eddbIds\">",tpath,"/systemsByEddbIds/:eddbIds</a><br/><br/>",
                                                                        "<a href=\"",tpath,"/systemsByEdsmId/:edsmIds\">",tpath,"/systemsByEdsmIds/:edsmIds</a><br/><br/>",
                                                                        "<a href=\"",tpath,"/stationsBySystemName/:systemNames\">",tpath,"/stationsBySystemName/:systemNames</a><br/><br/>",
                                                                        "<a href=\"",tpath,"/stationsByEddbId/:eddbIds\">",tpath,"/stationsByEddbId/:eddbIds</a><br/><br/>",
                                                                        "<a href=\"",tpath,"/systemsWithinLy/:systemName/:distance\">",tpath,"/systemsWithinLy/:systemName/:distance</a><br/><br/>",
                                                                        "<a href=\"",tpath,"/stationsWithinLy/:systemName/:distance\">",tpath,"/stationsWithinLy/:systemName/:distance</a><br/><br/>",
                                                                        "<br/><br/>Powered by <a href=\"http://hackage.haskell.org/package/scotty\">Scotty</a></body></html>"]

