{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Import.EDDB.Stations where

import EDDA.Types
import EDDA.Data.Database (getSystemEDDBIdsMap, saveStations, saveStationsCommodities)
import EDDA.Schema.Util (getStr, getInt, getIntArray, getDouble, getChr, getGuidance, getMount)
import EDDA.Data.Import.EDDB.Util
import EDDA.Data.Document (toDocument,valStr)

import Data.IORef
import Control.Monad (join)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAcceptEncoding)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO.Temp (withSystemTempFile)

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Csv as CSV
import Data.Int (Int32(..))
import Data.Char (intToDigit)
import Data.Maybe (fromJust)
import Data.List (foldl')
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Bson as B
import qualified Data.HashMap.Strict as HM

url = "https://eddb.io/archive/v4/stations.json"
module_url = "https://eddb.io/archive/v4/modules.json"
listings_url = "https://eddb.io/archive/v4/listings.csv"
commodities_url = "https://eddb.io/archive/v4/commodities.json"

type CommoditiesMap = HM.HashMap Int32 Str

downloadListings :: ConfigT [LC.ByteString]
downloadListings = do liftIO $ putStrLn "Downloading EDDB listings..."
                      result <- liftIO $ do
                           manager <- newManager tlsManagerSettings
                           initialRequest <- parseRequest listings_url
                           let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                           response <- httpLbs request manager 
                           let lines = LC.lines (responseBody response)
                           return $ tail lines
                      return result

downloadCommodities :: ConfigT (Maybe CommoditiesMap)
downloadCommodities =
                   do 
                      liftIO $ putStrLn "Downloading EDDB commodities..."
                      result <- liftIO $ do
                           manager <- newManager tlsManagerSettings
                           initialRequest <- parseRequest commodities_url
                           let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                           response <- httpLbs request manager 
                           let maybeCommodities = decode' (responseBody response) :: Maybe Value
                           case maybeCommodities of
                                Just commodities -> return $ toMap commodities
                                Nothing -> return Nothing
                      return result
                  where toMap (Array ar) = Just $ HM.fromList ((onlyJust . V.toList) (V.map valToKV ar))
                        toMap _ = Nothing
                        valToKV obj = do id <- getInt obj "id"
                                         name <- getStr obj "name"
                                         return (fromIntegral id,name)

downloadModules :: ConfigT (Maybe (HM.HashMap Int OutfittingModuleInfo))
downloadModules = do liftIO $ putStrLn "Downloading EDDB modules..."
                     result <- liftIO $ do
                           manager <- newManager tlsManagerSettings
                           initialRequest <- parseRequest module_url
                           let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                           response <- httpLbs request manager 
                           let modules = decode' (responseBody response) :: Maybe [Value]
                           return $ case modules of
                             Just modules -> modules
                             Nothing -> []
                     liftIO $ putStrLn "EDDB Modules downloaded"
                     return $ case allJust (map convertToModuleInfo result) of
                                    Just modules -> return $ HM.fromList modules
                                    Nothing -> Nothing
                  where convertToModuleInfo obj = do module_id <- getInt obj "id"
                                                     clsInt <- getInt obj "class"
                                                     let cls = intToDigit clsInt
                                                     rating <- getChr obj "rating"
                                                     doc <- case obj of
                                                                Object doc -> return doc
                                                                _ -> Nothing
                                                     group <- HM.lookup "group" doc
                                                     category <- getStr group "category"
                                                     name <- getStr group "name"
                                                     if category == "Bulkhead" then 
                                                         return (module_id,OutfittingModuleUtility { 
                                                             outfittingModuleUtilityName = name,
                                                             outfittingModuleUtilityClass = cls,
                                                             outfittingModuleUtilityRating = rating })
                                                     else if category == "Utility Mount" then 
                                                         return (module_id,OutfittingModuleUtility { 
                                                             outfittingModuleUtilityName = name,
                                                             outfittingModuleUtilityClass = cls,
                                                             outfittingModuleUtilityRating = rating })
                                                     else if category == "Weapon Hardpoint" then do
                                                         weapon_mode <- getMount obj "weapon_mode"
                                                         return (module_id,OutfittingModuleHardpoint { 
                                                             outfittingModuleHardpointName = name,
                                                             outfittingModuleHardpointMount = weapon_mode,
                                                             outfittingModuleHardpointGuidance = getGuidance obj "missle_type",
                                                             outfittingModuleHardpointClass = cls,
                                                             outfittingModuleHardpointRating = rating })
                                                     else if category == "Essential Equipment" then
                                                         return (module_id,OutfittingModuleStandard { 
                                                             outfittingModuleStandardName = name,
                                                             outfittingModuleStandardShip = getStr obj "ship",
                                                             outfittingModuleStandardClass = cls,
                                                             outfittingModuleStandardRating = rating })
                                                     else if category == "Internal Compartment" then
                                                         return (module_id,OutfittingModuleInternal { 
                                                             outfittingModuleInternalName = name,
                                                             outfittingModuleInternalClass = cls,
                                                             outfittingModuleInternalRating = rating })
                                                     else Nothing


mapModuleArray :: (HM.HashMap Int OutfittingModuleInfo) -> Str -> Str -> Value -> Maybe B.Field
mapModuleArray moduleMap from to obj = join $ (\sa -> (\ms -> ((toText to) B.:= (B.Array ms))) <$> outfittingsToDoc sa) <$> getIntArray obj from
                                       where moduleList :: [Int] -> Maybe [OutfittingModuleInfo]
                                             moduleList arr = allJust (map (\i -> HM.lookup i moduleMap) arr)
                                             outfittingsToDoc :: [Int] -> Maybe [B.Value]
                                             outfittingsToDoc arr = (map EDDA.Data.Document.toDocument) <$> (moduleList arr)

toDocument :: (HM.HashMap Int OutfittingModuleInfo) -> (HM.HashMap Int32 T.Text) -> Value -> Maybe (Str,Str,B.Document)
toDocument modulemap idmap obj = do !stationName <- getStr obj "name"
                                    !systemId <- getInt obj "system_id"
                                    !systemName <- HM.lookup (fromIntegral systemId) idmap >>= return . toStr
                                    !doc <- mapToDocument [ mapInt "id" "eddbId",
                                                           mapConst "systemName" (B.val (toText systemName)),
                                                           mapConst "stationName" (B.val (toText stationName)),
                                                           mapModuleArray modulemap "selling_modules" "outfitting",
                                                           mapStrNullable "type" "type",
                                                           mapStrNullable "state" "state",
                                                           mapStrNullable "faction" "faction",
                                                           mapStrNullable "government" "government",
                                                           mapStrNullable "allegiance" "allegiance",
                                                           mapIntNullable "distance_to_star" "distanceToStar",
                                                           mapStrArray "import_commodities" "importCommodities",
                                                           mapStrArray "export_commodities" "exportCommodities",
                                                           mapStrArray "prohibited_commodities" "prohibitedCommodities",
                                                           mapStrArray "economies" "economies",
                                                           mapStrArray "selling_ships" "ships",
                                                           mapBoolNullable "has_docking" "hasDocking",
                                                           mapBoolNullable "has_market" "hasMarket",
                                                           mapBoolNullable "has_shipyard" "hasShipyard",
                                                           mapBoolNullable "has_outfitting" "hasOutfitting",
                                                           mapBoolNullable "has_blackmarket" "hasBlackmarket",
                                                           mapBoolNullable "has_commodities" "hasCommodities",
                                                           mapBoolNullable "has_repair" "hasRepair",
                                                           mapBoolNullable "has_rearm" "hasRearm",
                                                           mapBoolNullable "has_refuel" "hasRefuel",
                                                           mapBoolNullable "is_planetary" "isPlanetary",
                                                           mapStrNullable "max_landing_pad_size" "maxLandingPadSize" ] obj 
                                    return (systemName,stationName,doc)

toDocumentList :: (HM.HashMap Int OutfittingModuleInfo) -> (HM.HashMap Int32 T.Text) -> V.Vector Value -> ConfigT (V.Vector (Maybe (Str,Str,B.Document)))
toDocumentList modulemap idmap stations = flip V.mapM stations (\v -> case EDDA.Data.Import.EDDB.Stations.toDocument modulemap idmap v of
                                                                        Just (!systemName,!stationName,!doc) -> return $ Just (systemName,stationName,doc)
                                                                        Nothing -> do liftIO $ C.putStrLn "Couldn't parse system: "
                                                                                      liftIO $ putStrLn (show v)
                                                                                      return Nothing)

saveToDatabase :: (HM.HashMap Int OutfittingModuleInfo) -> (HM.HashMap Int32 T.Text) -> V.Vector Value -> ConfigT ()
saveToDatabase modulemap idmap v = do 
                                      stations <- toDocumentList modulemap idmap v >>= return . onlyJustVec
                                      liftIO $ putStrLn "Importing into database..."
                                      saveStations $ V.toList stations
                                      liftIO $ putStrLn ("Stations imported: " ++ (show (V.length stations)))

convertAndSaveToDB :: (HM.HashMap Int OutfittingModuleInfo) -> (HM.HashMap Int32 T.Text) -> Config -> C.ByteString -> IO ()
convertAndSaveToDB modulemap idmap c d = 
                         do total <- newIORef 0 
                            streamParseIO 1000 d (saveToDB total)
                            totalCount <- readIORef total
                            putStrLn ("Total stations imported: " ++ (show totalCount))
           where substr d s e = C.concat ["[",(C.take (e-s-1) $ C.drop s d),"]"]
                 convert s = case (decodeStrict' s :: Maybe Value) of
                                Just (Array stations) -> return $ Just stations
                                Just _ -> return Nothing
                                Nothing -> return Nothing
                 saveToDB total d s e = do maybeStations <- (convert (substr d s e))
                                           case maybeStations of
                                                Just stations -> runReaderT (saveToDatabase modulemap idmap stations) c >> let totalCount = V.length stations in totalCount `seq` modifyIORef' total (+ totalCount)
                                                Nothing -> putStrLn "Couldn't decode a batch" >> C.putStrLn (substr d s e)

downloadAndImportStations :: ConfigT ()
downloadAndImportStations = do
                        liftIO $ putStrLn "Loading EDDB modules list"
                        !maybeModuleMap <- downloadModules
                        let !moduleMap = fromJust maybeModuleMap
                        liftIO $ putStrLn "Loading EDDB systems id map"
                        !maybeidmap <- getSystemEDDBIdsMap
                        let !idmap = fromJust maybeidmap
                        liftIO $ putStrLn ("EDDB systems id map loaded: " ++ (show (HM.size idmap)))
                        liftIO $ C.putStrLn "Downloading EDDB Stations data..."
                        r <- ask
                        liftIO $ withSystemTempFile "stations.json" (\f h -> download url "EDDB Stations data downloaded" f h >> C.readFile f >>= convertAndSaveToDB moduleMap idmap r)

insertListing cm e acc = 
                      if HM.member sId acc then HM.insert sId (doc:l) acc
                      else HM.insert sId [doc] acc
                      where
                          !sId = e V.! 1
                          !cId = e V.! 2
                          !cDemand = e V.! 6
                          !cBuyPrice = e V.! 4
                          !cSellPrice = e V.! 5
                          !cSupply = e V.! 3
                          !cName = cm HM.! cId
                          !doc = ["name" B.=: valStr cName,
                                 "buyPrice" B.=: B.val cBuyPrice,
                                 "sellPrice" B.=: B.val cSellPrice,
                                 "demand" B.=: B.val cDemand,
                                 "supply" B.=: B.val cSupply ]
                          l = acc HM.! sId

convertListings :: CommoditiesMap -> [LC.ByteString] -> HM.HashMap Int32 [B.Document]
convertListings cm v = foldl' f HM.empty v
        where f acc s = case CSV.decode CSV.NoHeader (LC.append s "\r\n") :: Either String (V.Vector (V.Vector Int32)) of
                            Right v -> acc `seq` v `seq` insertListing cm (v V.! 0) acc 
                            Left _ -> acc

downloadAndImportListings :: ConfigT ()
downloadAndImportListings = do maybeCommoditiesMap <- downloadCommodities 
                               case maybeCommoditiesMap of
                                    Just commoditiesMap -> do listings <- downloadListings
                                                              let commodities =  convertListings commoditiesMap listings
                                                              liftIO $ putStrLn "Importing station commodities"
                                                              saveStationsCommodities commodities
                                                              liftIO $ putStrLn ("Station commodities imported: " ++ (show (HM.size commodities)))
                                    Nothing -> liftIO $ putStrLn "Couldn't import commodities"

downloadAndImport :: ConfigT ()
downloadAndImport = downloadAndImportStations >> downloadAndImportListings
