{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Import.EDDB.Stations where

import EDDA.Types
import EDDA.Data.Database (query, getSystemEDDBIdsMap, saveStations, saveStationsCommodities)
import EDDA.Schema.Util (getStr, getInt, getIntArray, getDouble, getChr, getGuidance, getMount)
import EDDA.Data.Import.EDDB.Util
import EDDA.Data.Document (toDocument,valStr)

import Data.List.Split (chunksOf)
import Data.IORef
import Control.Applicative ((<|>))
import Control.Monad (join,forM_)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAcceptEncoding)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.IO.Temp (withSystemTempFile)
import System.IO.MMap (mmapFileByteString)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lazy.Search (replace)
import Data.ByteString.Search.Substitution
import qualified Data.Csv as CSV
import Data.Int (Int32(..))
import Data.Char (intToDigit)
import Data.Maybe (fromJust,fromMaybe)
import Data.List (foldl')
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Bson as B
import qualified Data.HashMap.Strict as HM

url = "https://eddb.io/archive/v6/stations.json"
moduleUrl = "https://eddb.io/archive/v6/modules.json"
listingsUrl = "https://eddb.io/archive/v6/listings.csv"
commoditiesUrl = "https://eddb.io/archive/v6/commodities.json"

type CommoditiesMap = HM.HashMap Int32 T.Text

downloadListings :: ConfigT LC.ByteString
downloadListings = do liftIO $ putStrLn "Downloading EDDB listings..."
                      liftIO $ do
                           manager <- newManager tlsManagerSettings
                           initialRequest <- parseRequest listingsUrl
                           let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                           response <- httpLbs request manager 
                           return $ responseBody response

downloadCommodities :: ConfigT (Maybe CommoditiesMap)
downloadCommodities =
                   do 
                      liftIO $ putStrLn "Downloading EDDB commodities..."
                      liftIO $ do
                           manager <- newManager tlsManagerSettings
                           initialRequest <- parseRequest commoditiesUrl
                           let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                           response <- httpLbs request manager 
                           let !maybeCommodities = decode' (responseBody response) :: Maybe Value
                           case maybeCommodities of
                                Just commodities -> return $ toMap commodities
                                Nothing -> return Nothing
                  where toMap (Array ar) = Just $ HM.fromList ((onlyJust . V.toList) (V.map valToKV ar))
                        toMap _ = Nothing
                        valToKV obj = do !id <- getInt obj "id"
                                         !name <- toText <$> getStr obj "name"
                                         return (fromIntegral id,name)

downloadModules :: ConfigT (Maybe (HM.HashMap Int OutfittingModuleInfo))
downloadModules = do liftIO $ putStrLn "Downloading EDDB modules..."
                     !result <- liftIO $ do
                           manager <- newManager tlsManagerSettings
                           initialRequest <- parseRequest moduleUrl
                           let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                           response <- httpLbs request manager 
                           let modules = decode' (responseBody response) :: Maybe [Value]
                           return $ fromMaybe [] modules
                     liftIO $ putStrLn "EDDB Modules downloaded"
                     case allJust (map convertToModuleInfo result) of
                       Just modules -> do liftIO $ putStrLn ("Modules loaded: "++ show (length modules))
                                          return $ Just (HM.fromList modules)
                       Nothing -> return Nothing
                  where
                        convertToModuleInfo obj = do module_id <- getInt obj "id"
                                                     clsInt <- getInt obj "class"
                                                     let cls = intToDigit clsInt
                                                     rating <- getChr obj "rating"
                                                     doc <- case obj of
                                                                Object doc -> return doc
                                                                _ -> Nothing
                                                     group <- HM.lookup "group" doc
                                                     category <- getStr group "category"
                                                     name <- getStr group "name"
                                                     if category == "Bulkhead" || category == "Utility Mount" then
                                                         return (module_id,OutfittingModuleUtility {
                                                             outfittingModuleUtilityName = name,
                                                             outfittingModuleUtilityClass = cls,
                                                             outfittingModuleUtilityRating = rating })
                                                     else if category == "Weapon Hardpoint" then do
                                                         weapon_mode <- getMount obj "weapon_mode" <|> Just Fixed
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


mapModuleArray :: HM.HashMap Int OutfittingModuleInfo -> Str -> Str -> Value -> Maybe B.Field
mapModuleArray moduleMap from to obj = fmap (\ms -> toText to B.:= B.Array ms) . outfittingsToDoc =<< getIntArray obj from
                                       where moduleList :: [Int] -> Maybe [OutfittingModuleInfo]
                                             moduleList arr = allJust (map (`HM.lookup` moduleMap) arr)
                                             outfittingsToDoc :: [Int] -> Maybe [B.Value]
                                             outfittingsToDoc arr = map EDDA.Data.Document.toDocument <$> moduleList arr

toDocument :: HM.HashMap Int OutfittingModuleInfo -> HM.HashMap Int32 T.Text -> Value -> Maybe (Str,Str,B.Document)
toDocument modulemap idmap obj = do !stationName <- getStr obj "name"
                                    !systemId <- getInt obj "system_id"
                                    !systemName <- toStr <$> HM.lookup (fromIntegral systemId) idmap
                                    !doc <- mapToDocument [mapInt "id" "eddbId",
                                                           mapConst "systemName" (B.val $! toText systemName),
                                                           mapConst "stationName" (B.val $! toText stationName),
                                                           mapModuleArray modulemap "selling_modules" "outfitting",
                                                           mapStrNullable "type" "type",
                                                           mapObjectArray "states" "states" (
                                                                mapToDocument [mapIntNullable "id" "id",
                                                                               mapStrNullable "name" "name"]
                                                           ),
                                                           mapIntNullable "controlling_minor_faction_id" "factionId",
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
                                                           mapStrNullable "max_landing_pad_size" "maxLandingPadSize"] obj
                                    return (systemName,stationName,doc)

toDocumentList :: HM.HashMap Int OutfittingModuleInfo -> HM.HashMap Int32 T.Text -> V.Vector Value -> ConfigT (V.Vector (Maybe (Str,Str,B.Document)))
toDocumentList modulemap idmap = V.mapM (\v -> case EDDA.Data.Import.EDDB.Stations.toDocument modulemap idmap v of
                                                    Just (!systemName,!stationName,!doc) -> return $ Just (systemName,stationName,doc)
                                                    Nothing -> do liftIO $ C.putStrLn "Couldn't parse system: " >> print v
                                                                  return Nothing)

saveToDatabase stations = do liftIO $ C.putStrLn "Importing into database..."
                             saveStations $ V.toList stations
                             liftIO $ C.putStrLn ("Stations imported: " `C.append` C.pack (show (V.length stations)))

convertAndSaveToDB :: HM.HashMap Int OutfittingModuleInfo -> HM.HashMap Int32 T.Text -> Config -> C.ByteString -> IO ()
convertAndSaveToDB modulemap idmap c d =
                         do total <- newIORef 0 
                            runReaderT (query (do context <- ask 
                                                  liftIO (streamParseIO 1000 d (saveToDB context total)))) c
                            totalCount <- readIORef total
                            putStrLn ("Total stations imported: " ++ show totalCount)
           where substr d s e = C.concat ["[",C.take (e-s-1) $ C.drop s d,"]"]
                 convert s = case (decodeStrict' s :: Maybe Value) of
                                Just (Array stations) -> return $ Just stations
                                Just _ -> return Nothing
                                Nothing -> return Nothing
                 saveToDB context total d s e =
                                        do maybeStations <- convert (substr d s e)
                                           case maybeStations of
                                                Just v -> do stations <- runReaderT (onlyJustVec <$> toDocumentList modulemap idmap v) c
                                                             runReaderT (saveToDatabase stations) context
                                                             let !totalCount = V.length stations in modifyIORef' total (+ totalCount)
                                                Nothing -> putStrLn "Couldn't decode a batch" >> C.putStrLn (substr d s e)

downloadAndImportStations :: ConfigT ()
downloadAndImportStations = do
                        liftIO $ putStrLn "Loading EDDB modules list"
                        !maybeModuleMap <- downloadModules
                        let !moduleMap = fromJust maybeModuleMap
                        liftIO $ putStrLn "Loading EDDB systems id map"
                        !maybeidmap <- getSystemEDDBIdsMap
                        let !idmap = fromJust maybeidmap
                        liftIO $ putStrLn ("EDDB systems id map loaded: " ++ show (HM.size idmap))
                        liftIO $ C.putStrLn "Downloading EDDB Stations data..."
                        r <- ask
                        liftIO $ withSystemTempFile "stations.json" (\f h -> download url "EDDB Stations data downloaded" f h >> mmapFileByteString f Nothing >>= convertAndSaveToDB moduleMap idmap r)

parseListings :: C.ByteString -> IO (V.Vector (VU.Vector Int32))
parseListings s = do let lines = tail $ LC.lines (LC.fromStrict s)
                     case allJust (map parseLine lines) of
                       Just vl -> return $ V.fromList vl
                       Nothing -> C.putStrLn "Couldn't import EDDB commodities" >> return V.empty
                  where parseLine :: LC.ByteString -> Maybe (VU.Vector Int32)
                        parseLine l = case CSV.decode CSV.NoHeader (replace (C.pack ",,") (C.pack ",0,") l) of
                                         Right v -> return (V.head v)
                                         Left msg -> unsafePerformIO (putStrLn msg >> LC.putStrLn l >> return Nothing)

listingsByStationId :: V.Vector (VU.Vector Int32) -> Int32 -> V.Vector (VU.Vector Int32)
listingsByStationId v stationId = V.filter (\e -> (e VU.! 1) == stationId) v

nameLabel :: Str
nameLabel = "name"

buyPriceLabel :: Str
buyPriceLabel = "buyPrice"

sellPriceLabel :: Str
sellPriceLabel = "sellPrice"

supplyLabel :: Str
supplyLabel = "supply"

demandLabel :: Str
demandLabel = "demand"

stationIdToDocument :: CommoditiesMap -> V.Vector (VU.Vector Int32) -> Int32 -> (Int32,V.Vector B.Document)
stationIdToDocument cm v stationId =
                      (stationId, V.map f (listingsByStationId v stationId))
                      where f e = doc
                                    where
                                      !sId = e VU.! 1
                                      !cId = e VU.! 2
                                      !cDemand = e VU.! 6
                                      !cBuyPrice = e VU.! 4
                                      !cSellPrice = e VU.! 5
                                      !cSupply = e VU.! 3
                                      !cName = cm HM.! cId
                                      !doc = [nameLabel B.=: B.val cName,
                                              buyPriceLabel B.=: B.val cBuyPrice,
                                              sellPriceLabel B.=: B.val cSellPrice,
                                              demandLabel B.=: B.val cDemand,
                                              supplyLabel B.=: B.val cSupply ]


convertListings :: CommoditiesMap -> V.Vector (VU.Vector Int32) -> [Int32] -> HM.HashMap Int32 (V.Vector B.Document)
convertListings cm v ids = HM.fromList $ map (stationIdToDocument cm v) ids

listingStationIds :: V.Vector (VU.Vector Int32) -> VU.Vector Int32
listingStationIds v = (VU.fromList . HS.toList) $ V.foldl' (\s v -> HS.insert (v VU.! 1) s) HS.empty v

downloadAndImportListings :: ConfigT ()
downloadAndImportListings = do maybeCommoditiesMap <- downloadCommodities
                               case maybeCommoditiesMap of
                                    Just cm -> do listings <- liftIO $ withSystemTempFile "listings.csv" (\f h -> download listingsUrl "EDDB commodities downloaded" f h >> mmapFileByteString f Nothing >>= parseListings)
                                                  let partitions = chunksOf 10 (VU.toList $ listingStationIds listings)
                                                  total <- liftIO $ newIORef 0
                                                  query (do context <- ask
                                                            liftIO $ forM_ partitions
                                                                         (\p -> do liftIO $ C.putStrLn "Importing station commodities"
                                                                                   let commodities = convertListings cm listings p
                                                                                   runReaderT (saveStationsCommodities commodities) context
                                                                                   liftIO $ C.putStrLn ("Station commodities imported: " `C.append` C.pack (show (HM.size commodities)))
                                                                                   liftIO $ let !totalCount = HM.size commodities in modifyIORef' total (+ totalCount)))
                                                  totalCount <- liftIO $ readIORef total
                                                  liftIO $ C.putStrLn ("Total station commodities imported: " `C.append` C.pack (show totalCount))
                                    Nothing -> liftIO $ C.putStrLn "Couldn't import commodities"

downloadAndImport :: ConfigT ()
downloadAndImport = downloadAndImportStations >> downloadAndImportListings
