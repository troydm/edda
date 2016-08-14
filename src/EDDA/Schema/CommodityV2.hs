{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.CommodityV2 where

import EDDA.Types
import EDDA.Data.Static (shipIdToName, commodityIdToName)
import EDDA.Schema.Util

import Control.Monad.Trans
import System.Log.Logger (errorM)
import Data.Maybe (isNothing)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

getCommodityId :: Value -> ConfigT (Maybe Str)
getCommodityId v = case getInt v "id" of
                        Just commodityId -> commodityIdToName commodityId
                        Nothing -> return $ getStr v "name"

getCommodity :: Value -> ConfigT (Maybe CommodityMarketInfo)
getCommodity v = do
                 ret <- do
                   let supplyLevel = getLevel v "supplyLevel"
                   let demandLevel = getLevel v "demandLevel"
                   maybeName <- getCommodityId v
                   return $ do name <- maybeName
                               buyPrice <- getInt v "buyPrice"
                               supply <- getInt v "supply"
                               sellPrice <- getInt v "sellPrice"
                               demand <- getInt v "demand"
                               return CommodityMarketInfo { commodityMarketInfoName = name, 
                                                            commodityMarketInfoBuyPrice = buyPrice,
                                                            commodityMarketInfoSupply = supply,
                                                            commodityMarketInfoSupplyLevel = supplyLevel,
                                                            commodityMarketInfoSellPrice = sellPrice,
                                                            commodityMarketInfoDemand = demand,
                                                            commodityMarketInfoDemandLevel = demandLevel }
                 if isNothing ret then liftIO (errorM "EDDA.Schema.CommodityV2" ("Couldn't parse commodity: " ++ (show v))) >> return Nothing else return ret 


getCommodities :: Value -> ConfigT (Maybe [CommodityMarketInfo])
getCommodities v = case getArray v "commodities" of
                     Just a -> allJust <$> sequence (map getCommodity a)
                     Nothing -> return Nothing


parseCommodity :: Value -> ConfigT (Maybe MessageInfo)
parseCommodity v = do
                    maybeCommodities <- getCommodities v
                    return $ do 
                               systemName <- getStr v "systemName"
                               stationName <- getStr v "stationName"
                               timestamp <- getTimestamp v "timestamp"
                               commodities <- maybeCommodities
                               Just $ CommodityInfo { commodityInfoSystemName = systemName, 
                                                      commodityInfoStationName = stationName, 
                                                      commodityInfoTimestamp = timestamp,
                                                      commodityInfoCommodities = HM.fromList (map (\v -> (commodityMarketInfoName v,v)) commodities) } 


