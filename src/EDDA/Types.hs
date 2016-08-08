module EDDA.Types where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Control.Monad.Trans.Reader
import Data.Time
import Data.Int (Int32(..))
import Data.Maybe (isJust,fromJust)

type Str = C.ByteString
type Timestamp = UTCTime

data Config = Config { zeroMqHost :: String,  mongoHost :: String, logPath :: String, mongoDb :: T.Text  } deriving Show

data Header = Header { headerUploaderId :: Str, headerSoftwareName :: Str, headerSoftwareVersion :: Str, headerGatewayTimestamp :: Maybe Timestamp } deriving Show

type Class = Char
type Rating = Char
data Level = Low | Med | High deriving Show
data Mount = Fixed | Gimballed | Turreted deriving Show
data Guidance = Dumbfire | Seeker deriving Show

data CommodityMarketInfo = CommodityMarketInfo {
            commodityMarketInfoName :: Str,
            commodityMarketInfoBuyPrice :: Int,
            commodityMarketInfoSupply :: Int,
            commodityMarketInfoSupplyLevel :: Maybe Level,
            commodityMarketInfoSellPrice :: Int,
            commodityMarketInfoDemand :: Int,
            commodityMarketInfoDemandLevel :: Maybe Level
    } deriving Show


data OutfittingModuleInfo = OutfittingModuleHardpoint { 
                                outfittingModuleHardpointName :: Str,
                                outfittingModuleHardpointMount :: Mount,
                                outfittingModuleHardpointGuidance :: Maybe Guidance,
                                outfittingModuleHardpointClass :: Class,
                                outfittingModuleHardpointRating :: Rating
                            } |
                            OutfittingModuleUtility { 
                                outfittingModuleUtilityName :: Str,
                                outfittingModuleUtilityClass :: Class,
                                outfittingModuleUtilityRating :: Rating
                            } |
                            OutfittingModuleStandard { 
                                outfittingModuleStandardName :: Str,
                                outfittingModuleStandardShip :: Maybe Str,
                                outfittingModuleStandardClass :: Class,
                                outfittingModuleStandardRating :: Rating
                            } |
                            OutfittingModuleInternal { 
                                outfittingModuleInternalName :: Str,
                                outfittingModuleInternalClass :: Class,
                                outfittingModuleInternalRating :: Rating
                            }
                            deriving Show

data MessageInfo = CommodityInfo { 
                        commodityInfoSystemName :: Str, 
                        commodityInfoStationName :: Str, 
                        commodityInfoTimestamp :: Timestamp, 
                        commodityInfoCommodities :: HM.HashMap Str CommodityMarketInfo
                   } | 
                   ShipyardInfo { 
                       shipyardInfoSystemName :: Str, 
                       shipyardInfoStationName :: Str, 
                       shipyardInfoTimestamp :: Timestamp, 
                       shipyardInfoShips :: HS.HashSet Str 
                   } | 
                   OutfittingInfo { 
                       outfittingInfoSystemName :: Str, 
                       outfittingInfoStationName :: Str, 
                       outfittingInfoTimestamp :: Timestamp, 
                       outfittingInfoModules :: HM.HashMap Str OutfittingModuleInfo
                   } 
                   deriving Show

data Ship = Ship { shipId :: Int, shipName :: Str }

data Ships = Ships { ships :: HS.HashSet Str, shipsTimestamp :: Timestamp }

data Commodities = Commodities { commodities :: HM.HashMap Str CommodityMarketInfo, commoditiesTimestamp :: Timestamp }

data Outfittings = Outfittings { outfitting :: HM.HashMap Str OutfittingModuleInfo, outfittingTimestamp :: Timestamp }

data Commodity = Commodity { 
        commodityId :: Int, 
        commodityCategory :: Str, 
        commodityName :: Str, 
        commodityAverage :: Int 
        }

data Outfitting = Outfitting { 
        outfittingId :: Int, 
        outfittingCategeroy :: Str, 
        outfittingName :: Str, 
        outfittingMount :: Str, 
        outfittingGuidance :: Str, 
        outfittingShip :: Str, 
        outfittingClass :: Int, 
        outfittingRating :: C.ByteString
        }

data SystemCoord = SystemCoord { systemCoordEdsmId :: Int32, systemCoordSystemName :: Str, systemCoordX :: Double, systemCoordY :: Double, systemCoordZ :: Double } deriving Show

type ConfigT = ReaderT Config IO

outfittingModuleName :: OutfittingModuleInfo -> Str
outfittingModuleName (OutfittingModuleUtility { outfittingModuleUtilityName = name } ) = name 
outfittingModuleName (OutfittingModuleHardpoint { outfittingModuleHardpointName = name } ) = name 
outfittingModuleName (OutfittingModuleStandard { outfittingModuleStandardName = name } ) = name 
outfittingModuleName (OutfittingModuleInternal { outfittingModuleInternalName = name } ) = name 

outfittingModuleClass :: OutfittingModuleInfo -> Class
outfittingModuleClass (OutfittingModuleUtility { outfittingModuleUtilityClass = cls } ) = cls 
outfittingModuleClass (OutfittingModuleHardpoint { outfittingModuleHardpointClass = cls } ) = cls 
outfittingModuleClass (OutfittingModuleStandard { outfittingModuleStandardClass = cls } ) = cls 
outfittingModuleClass (OutfittingModuleInternal { outfittingModuleInternalClass = cls } ) = cls 

outfittingModuleRating :: OutfittingModuleInfo -> Rating
outfittingModuleRating (OutfittingModuleUtility { outfittingModuleUtilityRating = rating } ) = rating 
outfittingModuleRating (OutfittingModuleHardpoint { outfittingModuleHardpointRating = rating } ) = rating 
outfittingModuleRating (OutfittingModuleStandard { outfittingModuleStandardRating = rating } ) = rating 
outfittingModuleRating (OutfittingModuleInternal { outfittingModuleInternalClass = rating } ) = rating 

outfittingModuleClassRating :: OutfittingModuleInfo -> Str
outfittingModuleClassRating m = C.pack ((outfittingModuleClass m) : [(outfittingModuleRating m)])

outfittingModuleFullName m = C.concat [outfittingModuleClassRating m,C.singleton ' ',outfittingModuleName m]

toText :: Str -> T.Text
toText = TE.decodeUtf8

toStr :: T.Text -> Str
toStr = TE.encodeUtf8

allJust :: [Maybe a] -> Maybe [a]
allJust l = if all isJust l then Just $ map fromJust l
            else Nothing

onlyJust :: [Maybe a] -> [a]
onlyJust l = map fromJust (filter isJust l)

onlyJustVec :: V.Vector (Maybe a) -> V.Vector a
onlyJustVec v = V.map fromJust (V.filter isJust v)

