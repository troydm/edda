{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.CommodityV3 where

import EDDA.Types
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

getCommodity :: Value -> ConfigT (Maybe CommodityMarketInfo)
getCommodity v = do
                 ret <- do
                   let statusFlags = getStrArray v "statusFlags"
                   return $ do name <- getStr v "name"
                               meanPrice <- getInt v "meanPrice"
                               buyPrice <- getInt v "buyPrice"
                               stock <- getInt v "stock"
                               let stockBracket = Low
                               stockBracket <- getBracket v "stockBracket"
                               sellPrice <- getInt v "sellPrice"
                               demand <- getInt v "demand"
                               let demandBracket = Low
                               demandBracket <- getBracket v "demandBracket"
                               return CommodityMarketInfo { commodityMarketInfoName = name,
                                                            commodityMarketInfoMeanPrice = meanPrice,
                                                            commodityMarketInfoBuyPrice = buyPrice,
                                                            commodityMarketInfoSupply = stock,
                                                            commodityMarketInfoSupplyLevel = stockBracket,
                                                            commodityMarketInfoSellPrice = sellPrice,
                                                            commodityMarketInfoDemand = demand,
                                                            commodityMarketInfoDemandLevel = demandBracket,
                                                            commodityMarketInfoStatusFlags = statusFlags }
                 if isNothing ret then liftIO (errorM "EDDA.Schema.CommodityV3" ("Couldn't parse commodity v3: " ++ (show v))) >> return Nothing else return ret 


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


