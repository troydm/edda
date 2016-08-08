{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.OutfittingV1 where

import EDDA.Types
import EDDA.Schema.Util

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Char8 as C


getModule :: Value -> ConfigT (Maybe OutfittingModuleInfo)
getModule v = return $ do 
                       category <- getStr v "category"
                       name <- getStr v "name"
                       cls <- getChr v "class"
                       rating <- getChr v "rating"
                       if category == "hardpoint" then do 
                           mount <- getMount v "mount"
                           return OutfittingModuleHardpoint { outfittingModuleHardpointName = name, 
                                                              outfittingModuleHardpointMount = mount, 
                                                              outfittingModuleHardpointGuidance = getGuidance v "guidance", 
                                                              outfittingModuleHardpointClass = cls, 
                                                              outfittingModuleHardpointRating = rating }
                       else if category == "utility" then 
                           return OutfittingModuleUtility { outfittingModuleUtilityName = name, 
                                                            outfittingModuleUtilityClass = cls, 
                                                            outfittingModuleUtilityRating = rating }
                       else if category == "standard" then 
                           return OutfittingModuleStandard { outfittingModuleStandardName = name, 
                                                             outfittingModuleStandardShip = getStr v "ship", 
                                                             outfittingModuleStandardClass = cls, 
                                                             outfittingModuleStandardRating = rating }
                       else if category == "internal" then 
                           return OutfittingModuleInternal { outfittingModuleInternalName = name, 
                                                             outfittingModuleInternalClass = cls, 
                                                             outfittingModuleInternalRating = rating }
                       else Nothing


getModules :: Value -> ConfigT (Maybe [OutfittingModuleInfo])
getModules v = case getArray v "modules" of
                     Just a -> allJust <$> sequence (map getModule a)
                     Nothing -> return Nothing


parseOutfitting :: Value -> ConfigT (Maybe MessageInfo)
parseOutfitting v = do
                    maybeModules <- getModules v
                    return $ do 
                               systemName <- getStr v "systemName"
                               stationName <- getStr v "stationName"
                               timestamp <- getTimestamp v "timestamp"
                               modules <- maybeModules
                               Just $ OutfittingInfo { outfittingInfoSystemName = systemName, 
                                                       outfittingInfoStationName = stationName, 
                                                       outfittingInfoTimestamp = timestamp,
                                                       outfittingInfoModules = HM.fromList (map (\v -> (outfittingModuleFullName v,v)) modules) } 



