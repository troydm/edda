{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.OutfittingV2 where
import EDDA.Types
import EDDA.Schema.Util

import System.Log.Logger
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Applicative ((<|>))
import Data.Maybe (isJust,isNothing)
import Data.Aeson
import Data.Aeson.Types
import Data.List (find)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Char8 as C

contains s sub = not $ null (T.breakOnAll sub s)

lookupVal t s = maybe Nothing (\(_,n) -> Just n) $ find (\(n,_) -> contains s n) t
lookupVal2 t s = maybe Nothing (\(_,_,n) -> Just n) $ find (\(n,m,_) -> (contains s n) && (contains s m)) t

mount :: Str -> Maybe Mount
mount = lookupVal
                [("_Fixed_",Fixed),
                 ("_Turret_",Turreted),
                 ("_Gimbal_",Gimballed)]

guidance :: Str -> Maybe Guidance
guidance = lookupVal
                [("Dumbfire",Dumbfire),
                 ("Seeker",Seeker),
                 ("Basic",Seeker)]

cls :: Str -> Maybe Class
cls = lookupVal
                [("_Size0_",'0'),
                 ("_Size1_",'1'),
                 ("_Size2_",'2'),
                 ("_Size3_",'3'),
                 ("_Size4_",'4'),
                 ("_Size5_",'5'),
                 ("_Size6_",'6'),
                 ("_Size7_",'7'),
                 ("_Size8_",'8')]

htpClass :: Str -> Maybe Class
htpClass = lookupVal
                [("_Tiny",'0'),
                 ("_Small",'1'),
                 ("_Medium",'2'),
                 ("_Large",'3'),
                 ("_Huge",'4'),
                 ("_",'1')]

htpName :: Str -> Maybe Str
htpName = lookupVal
                [("BasicMissileRack_","Missle Rack"),
                 ("DumbfireMissileRack_","Missle Rack"),
                 ("AdvancedTorpPylon_","Torpedo Pylon"),
                 ("Cannon_","Cannon"),
                 ("Slugshot_","Fragment Cannon"),
                 ("MineLauncher_","Mine Launcher"),
                 ("MiningLaser_","Mining Laser"),
                 ("PlasmaAccelerator_","Plasma Accelerator"),
                 ("Railgun_","Rail Gun"),
                 ("MultiCannon_","Multi-Cannon"),
                 ("BeamLaser_","Beam Laser"),
                 ("PulseLaser_","Pulse Laser"),
                 ("PulseLaserBurst_","Burst Laser")]

htpUtilityName :: Str -> Maybe Str
htpUtilityName = lookupVal
                [("CrimeScanner_","Kill Warrant Scanner"),
                 ("CloudScanner_","Frame Shift Wake Scanner"),
                 ("CargoScanner_","Cargo Scanner"),
                 ("ShieldBooster_","Shield Booster"),
                 ("PlasmaPointDefence_","Point Defence"),
                 ("ElectronicCountermeasure_","Electronic Countermeasure"),
                 ("HeatSinkLauncher_","Heat Sink Launcher"),
                 ("ChaffLauncher_","Chaff Launcher")]

htpRating :: Str -> Maybe Rating
htpRating = lookupVal2
                [("BeamLaser_","Fixed_Small",'E'),
                 ("BeamLaser_","Fixed_Medium",'D'),
                 ("BeamLaser_","Fixed_Large",'C'),
                 ("BeamLaser_","Fixed_Huge",'A'),
                 ("BeamLaser_","Gimbal_Small",'E'),
                 ("BeamLaser_","Gimbal_Medium",'D'),
                 ("BeamLaser_","Gimbal_Large",'C'),
                 ("BeamLaser_","Gimbal_Huge",'A'),
                 ("BeamLaser_","Turret_Small",'F'),
                 ("BeamLaser_","Turret_Medium",'E'),
                 ("BeamLaser_","Turret_Large",'D'),
                 ("PulseLaserBurst_","Fixed_Small",'F'),
                 ("PulseLaserBurst_","Fixed_Medium",'E'),
                 ("PulseLaserBurst_","Fixed_Large",'D'),
                 ("PulseLaserBurst_","Fixed_Huge",'A'),
                 ("PulseLaserBurst_","Gimbal_Small",'G'),
                 ("PulseLaserBurst_","Gimbal_Medium",'F'),
                 ("PulseLaserBurst_","Gimbal_Large",'E'),
                 ("PulseLaserBurst_","Gimbal_Huge",'A'),
                 ("PulseLaserBurst_","Turret_Small",'G'),
                 ("PulseLaserBurst_","Turret_Medium",'F'),
                 ("PulseLaserBurst_","Turret_Large",'E'),
                 ("PulseLaser_","Fixed_Small",'F'),
                 ("PulseLaser_","Fixed_Medium",'E'),
                 ("PulseLaser_","Fixed_Large",'D'),
                 ("PulseLaser_","Fixed_Huge",'A'),
                 ("PulseLaser_","Gimbal_Small",'G'),
                 ("PulseLaser_","Gimbal_Medium",'F'),
                 ("PulseLaser_","Gimbal_Large",'E'),
                 ("PulseLaser_","Gimbal_Huge",'A'),
                 ("PulseLaser_","Turret_Small",'G'),
                 ("PulseLaser_","Turret_Medium",'F'),
                 ("PulseLaser_","Turret_Large",'F'),
                 ("PlasmaAccelerator_","Fixed_Medium",'C'),
                 ("PlasmaAccelerator_","Fixed_Large",'B'),
                 ("PlasmaAccelerator_","Fixed_Huge",'A'),
                 ("Cannon_","Fixed_Small",'D'),
                 ("Cannon_","Fixed_Medium",'D'),
                 ("Cannon_","Fixed_Large",'C'),
                 ("Cannon_","Fixed_Huge",'B'),
                 ("Cannon_","Gimbal_Small",'E'),
                 ("Cannon_","Gimbal_Medium",'D'),
                 ("Cannon_","Gimbal_Large",'C'),
                 ("Cannon_","Gimbal_Huge",'B'),
                 ("Cannon_","Turret_Small",'F'),
                 ("Cannon_","Turret_Medium",'E'),
                 ("Cannon_","Turret_Large",'D'),
                 ("Railgun_","Fixed_Small",'D'),
                 ("Railgun_","Fixed_Medium",'B'),
                 ("MiningLaser_","Fixed_Small",'D'),
                 ("MiningLaser_","Fixed_Medium",'D'),
                 ("MineLauncher_","Fixed_Small",'I'),
                 ("MineLauncher_","Fixed_Medium",'I'),
                 ("MultiCannon_","Fixed_Small",'F'),
                 ("MultiCannon_","Fixed_Medium",'E'),
                 ("MultiCannon_","Fixed_Large",'C'),
                 ("MultiCannon_","Fixed_Huge",'A'),
                 ("MultiCannon_","Gimbal_Small",'G'),
                 ("MultiCannon_","Gimbal_Medium",'F'),
                 ("MultiCannon_","Gimbal_Large",'C'),
                 ("MultiCannon_","Gimbal_Huge",'A'),
                 ("MultiCannon_","Turret_Small",'G'),
                 ("MultiCannon_","Turret_Medium",'F'),
                 ("Slugshot_","Fixed_Small",'E'),
                 ("Slugshot_","Fixed_Medium",'A'),
                 ("Slugshot_","Fixed_Large",'C'),
                 ("Slugshot_","Gimbal_Small",'E'),
                 ("Slugshot_","Gimbal_Medium",'D'),
                 ("Slugshot_","Gimbal_Large",'C'),
                 ("Slugshot_","Turret_Small",'E'),
                 ("Slugshot_","Turret_Medium",'D'),
                 ("Slugshot_","Turret_Large",'C'),
                 ("AdvancedTorpPylon_","Fixed_Small",'I'),
                 ("AdvancedTorpPylon_","Fixed_Medium",'I'),
                 ("BasicMissileRack_","Fixed_Small",'B'),
                 ("BasicMissileRack_","Fixed_Medium",'B'),
                 ("DumbfireMissileRack_","Fixed_Small",'B'),
                 ("DumbfireMissileRack_","Fixed_Medium",'B'),
                 ("ElectronicCountermeasure_","Tiny",'F'),
                 ("HeatSinkLauncher_","Tiny",'I'),
                 ("ChaffLauncher_","Tiny",'I'),
                 ("PlasmaPointDefence_","Tiny",'I'),
                 ("DetailedSurfaceScanner_","Tiny",'C'),
                 ("DockingComputer_","_",'E'),
                 ("PlanetApproachSuite","_",'E'),
                 ("StellarBodyDiscoveryScanner_Standard","_",'E'),
                 ("StellarBodyDiscoveryScanner_Intermediate","_",'D'),
                 ("StellarBodyDiscoveryScanner_Advanced","_",'C')]

rating :: Str -> Maybe Rating
rating = lookupVal
                [("_Class1",'A'),
                 ("_Class2",'B'),
                 ("_Class3",'C'),
                 ("_Class4",'D'),
                 ("_Class5",'E')]

armourGrade :: Str -> Maybe Str
armourGrade = lookupVal
                [("_Grade1","Lightweight Alloys"),
                 ("_Grade2","Reinforced Alloys"),
                 ("_Grade3","Military Grade Composite"),
                 ("_Mirror","Mirrored Surface Composite"),
                 ("_Reactive","Reactive Surface Composite")] 

shipMap :: Str -> Maybe Str
shipMap = lookupVal
                [("SideWinder_","Sidewinder"),
                 ("Eagle_","Eagle"),
                 ("Hauler_","Hauler"),
                 ("Adder_","Adder"), 
                 ("Viper_MkIV","Viper MkIV"), 
                 ("Viper_","Viper Mk III"), 
                 ("CobraMkIII_","Cobra Mk III"), 
                 ("CobraMkIV_","Cobra MkIV"), 
                 ("Type6_","Type-6 Transporter"), 
                 ("Type7_","Type-7 Transporteer"), 
                 ("Type9_","Type-9 Transporteer"), 
                 ("Empire_Eagle","Imperial Eagle"), 
                 ("Empire_Courier","Imperial Courier"), 
                 ("Empire_Trader","Imperial Clipper"), 
                 ("Asp_Scout_","Asp Scout"), 
                 ("Asp_Explorer_","Asp Explorer"), 
                 ("Asp_","Asp Explorer"), 
                 ("Vulture_","Vulture"), 
                 ("Clipper_","Imperial Clipper"), 
                 ("Federation_Dropship_MkII","Federal Assault Ship"), 
                 ("Federation_Dropship","Federal Dropship"), 
                 ("Federation_Gunship","Federal Gunship"), 
                 ("FerDeLance_","Fer-de-Lance"), 
                 ("Orca_","Orca"), 
                 ("Python_","Python"), 
                 ("Anaconda_","Anaconda"), 
                 ("Federation_Corvette","Federal Corvette"), 
                 ("Cutter_","Imperial Cutter"), 
                 ("DiamondBack_","Diamondback Scout"),
                 ("DiamondBackXL_","Diamondback Explorer")]

intMap :: Str -> Maybe Str
intMap = lookupVal
                [("BuggyBay_","Planetary Vehicle Hangar"),
                 ("PlanetApproachSuite","Planetary Approach Suite"),
                 ("CargoRack_","Cargo Rack"),
                 ("CargoRack_","Cargo Rack"),
                 ("DetailedSurfaceScanner_","Detailed Surface Scanner"),
                 ("DockingComputer_","Standard Docking Computer"),
                 ("DroneControl_Collection_","Collector Limpet Controller"),
                 ("DroneControl_FuelTransfer_","Fuel Transfer Limpet Controller"),
                 ("DroneControl_Prospector_","Prospector Limpet Controller"),
                 ("DroneControl_ResourceSiphon_","Hatch Breaker Limpet Controller"),
                 ("FSDInterdictor_","Frame Shift Drive Interdictor"),
                 ("Engine_","Thrusters"),
                 ("FuelScoop_","Fuel Scoop"),
                 ("FuelTank_","Fuel Tank"),
                 ("HullReinforcement_","Hull Reinforcement Package"),
                 ("Hyperdrive_","Frame Shift Drive"),
                 ("PowerDistributor_","Power Distributor"),
                 ("Powerplant_","Power Plant"),
                 ("Refinery_","Refinery"),
                 ("LifeSupport_","Life Support"),
                 ("Repairer_","Auto Field-Maintenance Unit"),
                 ("Sensors_","Sensors"),
                 ("ShieldCellBank_","Shield Cell Bank"),
                 ("ShieldGenerator_","Shield Generator"),
                 ("StellarBodyDiscoveryScanner_Advanced","Advanced Discovery Scanner"),
                 ("StellarBodyDiscoveryScanner_Standard","Basic Discovery Scanner"),
                 ("StellarBodyDiscoveryScanner_Intermediate","Intermediate Discovery Scanner")]


parseArmour :: Str -> ConfigT (Maybe OutfittingModuleInfo)
parseArmour s = return $ do 
                   let ship = shipMap s
                   name <- armourGrade s
                   return OutfittingModuleStandard { outfittingModuleStandardName = name, 
                                                     outfittingModuleStandardShip = ship, 
                                                     outfittingModuleStandardClass = '1', 
                                                     outfittingModuleStandardRating = 'I' }

parseHardpoint :: Str -> ConfigT (Maybe OutfittingModuleInfo)
parseHardpoint s = return $ 
                    case htpName s of
                        Just name -> do 
                                       mount <- mount s
                                       cls <- htpClass s
                                       rating <- htpRating s
                                       return OutfittingModuleHardpoint { outfittingModuleHardpointName = name, 
                                                                          outfittingModuleHardpointMount = mount, 
                                                                          outfittingModuleHardpointGuidance = guidance s, 
                                                                          outfittingModuleHardpointClass = cls, 
                                                                          outfittingModuleHardpointRating = rating }
                        Nothing -> do 
                                     name <- htpUtilityName s
                                     cls <- cls s <|> htpClass s
                                     rating <- rating s <|> htpRating s
                                     return OutfittingModuleUtility { outfittingModuleUtilityName = name, 
                                                                      outfittingModuleUtilityClass = cls, 
                                                                      outfittingModuleUtilityRating = rating }

parseInternal :: Str -> ConfigT (Maybe OutfittingModuleInfo)
parseInternal s = return $ do 
                   name <- intMap s
                   cls <- cls s <|> htpClass s
                   rating <- rating s <|> htpRating s
                   return OutfittingModuleInternal { outfittingModuleInternalName = name, 
                                                     outfittingModuleInternalClass = cls, 
                                                     outfittingModuleInternalRating = rating }


getModule :: Str -> ConfigT (Maybe OutfittingModuleInfo)
getModule s = if contains s "_Armour_" then parseArmour s
              else if contains s "Hpt_" then parseHardpoint s
              else if contains s "Int_" then parseInternal s
              else return Nothing

getModule' :: Value -> ConfigT (Maybe OutfittingModuleInfo)
getModule' (String s) = do 
                          result <- getModule s
                          if isJust result then return result
                          else liftIO (errorM "EDDA.Schema.OutfittingV2" ("Couldn't parse commodity: " ++ (show s))) >> return Nothing 
getModule' _ = return Nothing


getModules :: Value -> ConfigT (Maybe [OutfittingModuleInfo])
getModules v = case getArray v "modules" of
                     Just a -> allJust <$> sequence (map getModule' a)
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



