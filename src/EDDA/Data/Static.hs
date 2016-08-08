{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Static where

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import EDDA.Types

import Database.MongoDB
import Data.Bson ((=:))
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import EDDA.Data.Database (query)

parseShip :: Document -> Maybe Ship
parseShip doc = do
                  shipId <- doc !? "id"
                  name <- doc !? "name"
                  return Ship { shipId = shipId, shipName = TE.encodeUtf8 name }

getShip :: Int -> ConfigT (Maybe Ship)
getShip shipId = do 
                ship <- query (findOne (select ["id" =: shipId] "shipyard"))
                case ship of
                    Just ship -> return $ parseShip ship
                    Nothing -> return Nothing

shipIdToName :: Int -> ConfigT (Maybe Str)
shipIdToName shipId = do ship <- getShip shipId
                         case ship of
                           Just ship -> return $ Just (shipName ship)
                           Nothing -> return Nothing

parseCommodity :: Document -> Maybe Commodity
parseCommodity doc = do commodityId <- doc !? "id"
                        category <- doc !? "category"
                        name <- doc !? "name"
                        avg <- doc !? "average"
                        return Commodity { commodityId = commodityId, commodityCategory = TE.encodeUtf8 category, commodityName = TE.encodeUtf8 name, commodityAverage = avg }

getCommodity :: Int -> ConfigT (Maybe Commodity)
getCommodity commodityId = 
             do commodity <- query (findOne (select ["id" =: commodityId] "commodity"))
                case commodity of
                    Just commodity -> return $ parseCommodity commodity
                    Nothing -> return Nothing

commodityIdToName :: Int -> ConfigT (Maybe Str)
commodityIdToName commodityId = do commodity <- getCommodity commodityId
                                   case commodity of
                                    Just commodity -> return $ Just (commodityName commodity)
                                    Nothing -> return Nothing
