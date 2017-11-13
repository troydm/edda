{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.ShipyardV1 where

import EDDA.Types
import EDDA.Schema.Util

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS


getShips :: Value -> ConfigT (Maybe [Str])
getShips v = return $ getStrArray v "ships"

parseShipyard :: Value -> ConfigT (Maybe MessageInfo)
parseShipyard v = do
                    ships <- getShips v
                    return $ do 
                               systemName <- getStr v "systemName"
                               stationName <- getStr v "stationName"
                               timestamp <- getTimestamp v "timestamp"
                               s <- ships
                               Just $ ShipyardInfo { shipyardInfoSystemName = systemName, 
                                                     shipyardInfoStationName = stationName, 
                                                     shipyardInfoTimestamp = timestamp,
                                                     shipyardInfoShips = HS.fromList s } 

