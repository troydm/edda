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

contains s sub = not $ null (T.breakOnAll (T.toLower sub) (T.toLower s))

lookupVal t s = (\(_,n) -> Just n) =<< find (\(n,_) -> contains s n) t
lookupVal2 t s = (\(_,_,n) -> Just n) =<< find (\(n,m,_) -> contains s n && contains s m) t

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
                 ("ATDumbfireMissile_","AX Missle Rack"),
                 ("DrunkMissileRack_","Pack-Hound Missle Rack"),
                 ("CausticMissile_","Enzyme Missle Rack"),
                 ("AdvancedTorpPylon_","Torpedo Pylon"),
                 ("Cannon_","Cannon"),
                 ("Slugshot_","Fragment Cannon"),
                 ("MineLauncher_","Mine Launcher"),
                 ("Mining_AbrBlstr_","Abrasion Blaster"),
                 ("Mining_SeismChrgWarhd","Seismic Charge Launcher"),
                 ("Mining_SubsurfDispMisle","Sub-surface Displacement Missle"),
                 ("MiningLaser_","Mining Laser"),
                 ("Guardian_PlasmaLauncher_","Guardian Plasma Launcher"),
                 ("PlasmaAccelerator_","Plasma Accelerator"),
                 ("FlakMortar_","Remote Release Flak Launcher"),
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
                 ("FlechetteLauncher_","Flechette Launcher"),
                 ("MraScanner_","Pulse Wave Analyser"),
                 ("XenoScanner_","Xeno Scanner"),
                 ("AntiUnknownShutdown_","Shutdown Field Neutraliser"),
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
                 ("DrunkMissileRack_","Fixed_Medium",'B'),
                 ("CausticMissile_","Fixed_Medium",'B'),
                 ("FlechetteLauncher_","Fixed_Medium",'A'),
                 ("FlechetteLauncher_","Turret_Medium",'A'),
                 ("Mining_AbrBlstr_","Fixed_Small",'D'),
                 ("Mining_AbrBlstr_","Turret_Small",'D'),
                 ("Mining_SeismChrgWarhd","Fixed_Medium",'B'),
                 ("Mining_SeismChrgWarhd","Turret_Medium",'B'),
                 ("Mining_SubsurfDispMisle","Fixed_Small",'D'),
                 ("Mining_SubsurfDispMisle","Fixed_Medium",'B'),
                 ("Mining_SubsurfDispMisle","Turret_Small",'D'),
                 ("Mining_SubsurfDispMisle","Turret_Medium",'B'),
                 ("Guardian_PlasmaLauncher_","Fixed_Small",'D'),
                 ("Guardian_PlasmaLauncher_","Fixed_Medium",'B'),
                 ("Guardian_PlasmaLauncher_","Fixed_Large",'C'),
                 ("Guardian_PlasmaLauncher_","Turret_Small",'D'),
                 ("Guardian_PlasmaLauncher_","Turret_Medium",'B'),
                 ("Guardian_PlasmaLauncher_","Turret_Large",'C'),
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
                 ("FlakMortar_","Fixed_Medium",'B'),
                 ("FlakMortar_","Turret_Medium",'B'),
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
                 ("MiningLaser_","Turret_Small",'D'),
                 ("MiningLaser_","Turret_Medium",'D'),
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
                 ("AdvancedTorpPylon_","Fixed_Large",'I'),
                 ("BasicMissileRack_","Fixed_Small",'A'),
                 ("BasicMissileRack_","Fixed_Medium",'A'),
                 ("BasicMissileRack_","Fixed_Large",'A'),
                 ("DumbfireMissileRack_","Fixed_Small",'A'),
                 ("DumbfireMissileRack_","Fixed_Medium",'A'),
                 ("DumbfireMissileRack_","Fixed_Large",'A'),
                 ("ATDumbfireMissile_","Fixed_Medium",'B'),
                 ("ATDumbfireMissile_","Fixed_Large",'A'),
                 ("ATDumbfireMissile_","Turret_Medium",'B'),
                 ("ATDumbfireMissile_","Turret_Large",'A'),
                 ("ElectronicCountermeasure_","Tiny",'F'),
                 ("HeatSinkLauncher_","Tiny",'I'),
                 ("ChaffLauncher_","Tiny",'I'),
                 ("PlasmaPointDefence_","Tiny",'I'),
                 ("DetailedSurfaceScanner_","Tiny",'C'),
                 ("XenoScanner_","Tiny",'E'),
                 ("DockingComputer_","_",'E'),
                 ("PlanetApproachSuite","_",'E'),
                 ("AntiUnknownShutdown_","Tiny",'F'),
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
                 ("Viper_","Viper MkIII"), 
                 ("CobraMkIII_","Cobra MkIII"), 
                 ("CobraMkIV_","Cobra MkIV"), 
                 ("Type6_","Type-6 Transporter"), 
                 ("Type7_","Type-7 Transporteer"), 
                 ("Type9_","Type-9 Heavy"), 
                 ("Empire_Eagle","Imperial Eagle"), 
                 ("Empire_Courier","Imperial Courier"), 
                 ("Empire_Trader","Imperial Clipper"), 
                 ("Asp_Scout_","Asp Scout"), 
                 ("Asp_Explorer_","Asp Explorer"), 
                 ("Asp_","Asp Explorer"), 
                 ("Vulture_","Vulture"), 
                 ("Empire_Trader_","Imperial Clipper"), 
                 ("Clipper_","Imperial Clipper"), 
                 ("Federation_Dropship_MkII","Federal Assault Ship"), 
                 ("Federation_Dropship","Federal Dropship"), 
                 ("Federation_Gunship","Federal Gunship"), 
                 ("FerDeLance_","Fer-de-Lance"), 
                 ("Orca_","Orca"), 
                 ("Python_","Python"), 
                 ("FerDeLance_","Fer-de-Lance"), 
                 ("Anaconda_","Anaconda"), 
                 ("Federation_Corvette","Federal Corvette"), 
                 ("Cutter_","Imperial Cutter"), 
                 ("Independant_Trader","Keelback"),
                 ("DiamondBack_","Diamondback Scout"),
                 ("DiamondBackXL_","Diamondback Explorer")]

intRatingMap :: Str -> Maybe Rating
intRatingMap = lookupVal2
                [("FighterBay_","_",'D'),
                 ("SupercruiseAssist","_",'E'),
                 ("GuardianFSDBooster","_",'H'),
                 ("GuardianPowerDistributor","_",'A'),
                 ("GuardianPowerPlant","_",'A'),
                 ("DroneControl_","UnkVesselResearch",'E'),
                 ("PassengerCabin_","_Class1",'E'),
                 ("PassengerCabin_","_Class2",'D'),
                 ("PassengerCabin_","_Class3",'C'),
                 ("PassengerCabin_","_Class4",'B')]

int2Map :: Str -> Maybe Str
int2Map = lookupVal2
                [("PassengerCabin_","_Class1","Economy Passenger Cabin"),
                 ("PassengerCabin_","_Class2","Business Passenger Cabin"),
                 ("PassengerCabin_","_Class3","First Class Passenger Cabin"),
                 ("PassengerCabin_","_Class4","Luxury Passenger Cabin")]

intMap :: Str -> Maybe Str
intMap = lookupVal
                [("BuggyBay_","Planetary Vehicle Hangar"),
                 ("PlanetApproachSuite","Planetary Approach Suite"),
                 ("CargoRack_","Cargo Rack"),
                 ("GuardianFSDBooster_","Guardian FSD Booster"),
                 ("GuardianPowerDistributor_","Guardian Power Distributor"),
                 ("GuardianPowerPlant_","Guardian Power Plant"),
                 ("GuardianShieldReinforcement_","Guardian Shield Reinforcement"),
                 ("DetailedSurfaceScanner_","Detailed Surface Scanner"),
                 ("DockingComputer_","Standard Docking Computer"),
                 ("DroneControl_Collection_","Collector Limpet Controller"),
                 ("DroneControl_Decontamination_","Decontamination Limpet Controller"),
                 ("DroneControl_FuelTransfer_","Fuel Transfer Limpet Controller"),
                 ("DroneControl_Prospector_","Prospector Limpet Controller"),
                 ("DroneControl_ResourceSiphon_","Hatch Breaker Limpet Controller"),
                 ("DroneControl_Repair_","Repair Limpet Controller"),
                 ("DroneControl_Recon_","Recon Limpet Controller"),
                 ("DroneControl_UnkVesselResearch","Research Limpet Controller"),
                 ("FSDInterdictor_","Frame Shift Drive Interdictor"),
                 ("SupercruiseAssist","Supercruise Assist"),
                 ("Engine_","Thrusters"),
                 ("FuelScoop_","Fuel Scoop"),
                 ("FuelTank_","Fuel Tank"),
                 ("HullReinforcement_","Hull Reinforcement Package"),
                 ("ModuleReinforcement_","Module Reinforcement Package"),
                 ("Hyperdrive_","Frame Shift Drive"),
                 ("PowerDistributor_","Power Distributor"),
                 ("Powerplant_","Power Plant"),
                 ("Refinery_","Refinery"),
                 ("LifeSupport_","Life Support"),
                 ("Repairer_","Auto Field-Maintenance Unit"),
                 ("FighterBay_","Fighter Hangar"),
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
                   name <- int2Map s <|> intMap s
                   cls <- cls s <|> htpClass s
                   rating <- intRatingMap s <|> rating s <|> htpRating s
                   return OutfittingModuleInternal { outfittingModuleInternalName = name,
                                                     outfittingModuleInternalClass = cls,
                                                     outfittingModuleInternalRating = rating }


getModule :: Str -> ConfigT (Maybe OutfittingModuleInfo)
getModule s
    | contains s "_Armour_" = parseArmour s
    | contains s "Hpt_" = parseHardpoint s
    | contains s "Int_" = parseInternal s
    | otherwise = return Nothing

getModule' :: Value -> ConfigT (Maybe OutfittingModuleInfo)
getModule' (String s) = do
                          result <- getModule s
                          if isJust result then return result
                          else liftIO (errorM "EDDA.Schema.OutfittingV2" ("Couldn't parse outfitting v2: " ++ show s)) >> return Nothing
getModule' _ = return Nothing


getModules :: Value -> ConfigT (Maybe [OutfittingModuleInfo])
getModules v = case getArray v "modules" of
                     Just a -> allJust <$> mapM getModule' a
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



