{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Document where

import Prelude hiding (lookup)

import EDDA.Types

import Control.Monad (join)
import Data.Maybe (maybeToList)
import Data.Bson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

valStr s = val $ TE.decodeUtf8 s
valChr c = valStr $ C.singleton c

valLevel High = valStr "High"
valLevel Med = valStr "Med"
valLevel Low = valStr "Low"

toLevel (String "High") = Just High
toLevel (String "Med") = Just Med
toLevel (String "Low") = Just Low
toLevel _ = Nothing
lookupLevel l doc = join $ toLevel <$> lookup l doc

valMount Fixed = valStr "Fixed"
valMount Gimballed = valStr "Gimballed"
valMount Turreted = valStr "Turreted"

toMount (String "Fixed") = Just Fixed
toMount (String "Gimballed") = Just Gimballed
toMount (String "Turreted") = Just Turreted
toMount _ = Nothing
lookupMount l doc = join $ toMount <$> lookup l doc

valGuidance Dumbfire = valStr "Dumbfire"
valGuidance Seeker = valStr "Seeker"

toGuidance (String "Dumbfire") = Just Dumbfire
toGuidance (String "Seeker") = Just Seeker
toGuidance _ = Nothing
lookupGuidance l doc = join $ toGuidance <$> lookup l doc

class ToDocument a where
    toDocument :: a -> Value

class FromDocument a where
    fromDocument :: Value -> Maybe a

instance ToDocument Ships where
    toDocument (Ships {ships = ships, shipsTimestamp = timestamp}) = 
        Doc ["ships" := shipsList, "shipsTimestamp" := val timestamp]
        where shipsList = Array $ map (\s -> val (TE.decodeUtf8 s)) (HS.toList ships)

instance FromDocument Ships where
    fromDocument (Doc doc) = do (Array ships) <- lookup "ships" doc
                                timestamp <- lookup "shipsTimestamp" doc
                                return Ships { ships = HS.fromList (map (\(String s) -> toStr s) ships), shipsTimestamp = timestamp}
    fromDocument _ = Nothing

instance ToDocument Commodities where
    toDocument (Commodities {commodities = commodities, commoditiesTimestamp = timestamp}) = 
        Doc ["commodities" := Array (map commodityDoc (HM.elems commodities)), "commoditiesTimestamp" := val timestamp]
        where commodityDoc (CommodityMarketInfo { 
                            commodityMarketInfoName = name,
                            commodityMarketInfoBuyPrice = buyPrice,
                            commodityMarketInfoSupply = supply,
                            commodityMarketInfoSupplyLevel = supplyLevel,
                            commodityMarketInfoSellPrice = sellPrice,
                            commodityMarketInfoDemand = demand,
                            commodityMarketInfoDemandLevel = demandLevel }) = 
                              Doc $ ["name" := valStr name, 
                                     "buyPrice" := val buyPrice,
                                     "supply" := val supply, 
                                     "sellPrice" := val sellPrice,
                                     "demand" := val demand] ++ 
                                     maybeToList ((\l -> "supplyLevel" := valLevel l) <$> supplyLevel) ++
                                     maybeToList ((\l -> "demandLevel" := valLevel l) <$> demandLevel)

instance FromDocument Commodities where
    fromDocument (Doc doc) = do (Array maybeCommodities) <- lookup "commodities" doc
                                timestamp <- lookup "commoditiesTimestamp" doc
                                commodities <- allJust $ map toCommodity maybeCommodities
                                return Commodities { commodities = HM.fromList (map (\v -> (commodityMarketInfoName v,v)) commodities), commoditiesTimestamp = timestamp }
                             where  toCommodity (Doc doc) = 
                                                     do (String name) <- lookup "name" doc
                                                        (Int32 buyPrice) <- lookup "buyPrice" doc
                                                        (Int32 supply) <- lookup "supply" doc
                                                        (Int32 sellPrice) <- lookup "sellPrice" doc
                                                        (Int32 demand) <- lookup "demand" doc
                                                        return CommodityMarketInfo { commodityMarketInfoName = toStr name,
                                                                                     commodityMarketInfoBuyPrice = fromIntegral buyPrice,
                                                                                     commodityMarketInfoSupply = fromIntegral supply,
                                                                                     commodityMarketInfoSellPrice = fromIntegral sellPrice,
                                                                                     commodityMarketInfoDemand = fromIntegral demand,
                                                                                     commodityMarketInfoSupplyLevel = lookupLevel "supplyLevel" doc,
                                                                                     commodityMarketInfoDemandLevel = lookupLevel "demandLevel" doc } 
                                    toCommodity _ = Nothing
                                
    fromDocument _ = Nothing

instance ToDocument Outfittings where
    toDocument (Outfittings { outfitting = outfitting, outfittingTimestamp = timestamp}) = 
        Doc ["outfitting" := Array (map toDocument (HM.elems outfitting)), "outfittingTimestamp" := val timestamp]

instance ToDocument OutfittingModuleInfo where
        toDocument (OutfittingModuleHardpoint { 
                                outfittingModuleHardpointName = name,
                                outfittingModuleHardpointMount = mount,
                                outfittingModuleHardpointGuidance = guidance,
                                outfittingModuleHardpointClass = cls,
                                outfittingModuleHardpointRating = rating
                            }) = Doc $ ["category" := valStr "hardpoint", "name" := valStr name, "mount" := valMount mount,
                                        "class" := valChr cls, "rating" := valChr rating ] ++ 
                                        maybeToList ((\g -> "guidance" := valGuidance g) <$> guidance)
        toDocument (OutfittingModuleUtility { 
                               outfittingModuleUtilityName = name,
                               outfittingModuleUtilityClass = cls,
                               outfittingModuleUtilityRating = rating
                           }) = Doc $ ["category" := valStr "utility", "name" := valStr name, "class" := valChr cls, "rating" := valChr rating]
        toDocument (OutfittingModuleStandard { 
                               outfittingModuleStandardName = name,
                               outfittingModuleStandardShip = maybeShip,
                               outfittingModuleStandardClass = cls,
                               outfittingModuleStandardRating = rating
                           }) = Doc $ ["category" := valStr "standard", "name" := valStr name, "class" := valChr cls, "rating" := valChr rating] ++
                                        maybeToList ((\s -> "ship" := valStr s) <$> maybeShip)
        toDocument (OutfittingModuleInternal { 
                               outfittingModuleInternalName = name,
                               outfittingModuleInternalClass = cls,
                               outfittingModuleInternalRating = rating
                           }) = Doc $ ["category" := valStr "internal", "name" := valStr name, "class" := valChr cls, "rating" := valChr rating]

instance FromDocument Outfittings where
    fromDocument (Doc doc) = do (Array maybeOutfittings) <- lookup "outfitting" doc
                                timestamp <- lookup "outfittingTimestamp" doc
                                outfittings <- allJust $ map toOutfitting maybeOutfittings
                                return Outfittings { outfitting = HM.fromList (map (\v -> (outfittingModuleFullName v,v)) outfittings), outfittingTimestamp = timestamp }
                           where toOutfitting (Doc doc) = lookup "category" doc >>= toOutfittingModule doc
                                 toOutfitting _ = Nothing
                                 toOutfittingModule doc (String "hardpoint") = do (String name) <- lookup "name" doc
                                                                                  mount <- lookupMount "mount" doc
                                                                                  (String cls) <- lookup "class" doc
                                                                                  (String rating) <- lookup "rating" doc
                                                                                  return OutfittingModuleHardpoint { 
                                                                                            outfittingModuleHardpointName = toStr name,
                                                                                            outfittingModuleHardpointMount = mount,
                                                                                            outfittingModuleHardpointGuidance = lookupGuidance "guidance" doc,
                                                                                            outfittingModuleHardpointClass = T.head cls,
                                                                                            outfittingModuleHardpointRating = T.head rating }
                                 toOutfittingModule doc (String "utility") = do (String name) <- lookup "name" doc
                                                                                (String cls) <- lookup "class" doc
                                                                                (String rating) <- lookup "rating" doc
                                                                                return OutfittingModuleUtility { 
                                                                                            outfittingModuleUtilityName = toStr name,
                                                                                            outfittingModuleUtilityClass = T.head cls,
                                                                                            outfittingModuleUtilityRating = T.head rating }
                                 toOutfittingModule doc (String "standard") = do (String name) <- lookup "name" doc
                                                                                 (String cls) <- lookup "class" doc
                                                                                 (String rating) <- lookup "rating" doc
                                                                                 return OutfittingModuleStandard { 
                                                                                            outfittingModuleStandardName = toStr name,
                                                                                            outfittingModuleStandardShip = (\(String s) -> toStr s) <$> lookup "ship" doc,
                                                                                            outfittingModuleStandardClass = T.head cls,
                                                                                            outfittingModuleStandardRating = T.head rating }
                                 toOutfittingModule doc (String "internal") = do (String name) <- lookup "name" doc
                                                                                 (String cls) <- lookup "class" doc
                                                                                 (String rating) <- lookup "rating" doc
                                                                                 return OutfittingModuleInternal { 
                                                                                            outfittingModuleInternalName = toStr name,
                                                                                            outfittingModuleInternalClass = T.head cls,
                                                                                            outfittingModuleInternalRating = T.head rating }
                                 toOutfittingModule _ _ = Nothing

                                
    fromDocument _ = Nothing

instance FromDocument SystemCoord where
    fromDocument (Doc doc) = do (Int32 edsmId) <- lookup "edsmId" doc
                                (String systemName) <- lookup "systemName" doc
                                (Float x) <- lookup "x" doc
                                (Float y) <- lookup "y" doc
                                (Float z) <- lookup "z" doc
                                return $ SystemCoord edsmId systemName x y z
    fromDocument _ = Nothing

