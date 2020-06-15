{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.Parser where

import EDDA.Types
import EDDA.Schema.Util

import Control.Applicative ((<|>))
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Bson (Field(..))
import qualified Data.Bson as BSON
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Time.ISO8601 (parseISO8601) 

import qualified EDDA.Schema.ShipyardV1 as ShipyardV1
import qualified EDDA.Schema.ShipyardV2 as ShipyardV2
import qualified EDDA.Schema.OutfittingV1 as OutfittingV1
import qualified EDDA.Schema.OutfittingV2 as OutfittingV2
import qualified EDDA.Schema.CommodityV2 as CommodityV2
import qualified EDDA.Schema.CommodityV3 as CommodityV3


parseHeader :: Value -> ConfigT (Maybe Header)
parseHeader (Object v) = do
            case HM.lookup "header" v of
                    Just h -> return $ do
                                         uploaderId <- getStr h "uploaderID" <|> Just ""
                                         softwareName <- getStr h "softwareName"
                                         softwareVersion <- getStr h "softwareVersion"
                                         return Header { headerUploaderId = uploaderId,
                                                         headerSoftwareName = softwareName,
                                                         headerSoftwareVersion = softwareVersion,
                                                         headerGatewayTimestamp = getTimestamp h "gatewayTimestamp" }
                    Nothing -> return Nothing
parseHeader v = showValue v >> return Nothing

parseMessage :: Value -> ConfigT (Maybe MessageInfo)
parseMessage v = case message v of
                    Just m ->  case schemaRef v of
                                 Just ref -> if ref == "https://eddn.edcd.io/schemas/journal/1" then return $ Just IgnoreInfo
                                             else if ref == "https://eddn.edcd.io/schemas/shipyard/1" then ShipyardV1.parseShipyard m
                                             else if ref == "https://eddn.edcd.io/schemas/shipyard/2" then ShipyardV2.parseShipyard m
                                             else if ref == "https://eddn.edcd.io/schemas/commodity/2" then CommodityV2.parseCommodity m
                                             else if ref == "https://eddn.edcd.io/schemas/commodity/3" then CommodityV3.parseCommodity m
                                             else if ref == "https://eddn.edcd.io/schemas/outfitting/1" then OutfittingV1.parseOutfitting m
                                             else if ref == "https://eddn.edcd.io/schemas/outfitting/2" then OutfittingV2.parseOutfitting m
                                             else if "test" `T.isSuffixOf` ref then return $ Just IgnoreInfo
                                             else return Nothing
                                 Nothing -> return Nothing
                    Nothing -> return Nothing

