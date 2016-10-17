module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import System.IO (stdout)
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Formatter (simpleLogFormatter)

import EDDA.Schema.ParserTest (parserTests)
import EDDA.Schema.CommodityV2Test (commodityV2Tests)
import EDDA.Schema.CommodityV3Test (commodityV3Tests)
import EDDA.Schema.OutfittingV1Test (outfittingV1Tests)
import EDDA.Schema.OutfittingV2Test (outfittingV2Tests)
import EDDA.Schema.ShipyardV1Test (shipyardV1Tests)
import EDDA.Schema.ShipyardV2Test (shipyardV2Tests)

tests = hUnitTestToTests $ TestList (parserTests ++ commodityV2Tests ++ commodityV3Tests ++ outfittingV1Tests 
                                     ++ outfittingV2Tests ++ shipyardV1Tests ++ shipyardV2Tests) 

setRootLogger :: IO ()
setRootLogger = do handler <- fileHandler "edda-test.log" DEBUG >>= 
                            \lh -> return $ setFormatter lh (simpleLogFormatter "$prio:$loggername:$time - $msg")
                   updateGlobalLogger rootLoggerName (addHandler handler)
                   updateGlobalLogger rootLoggerName (setLevel INFO)

main = do setRootLogger
          defaultMain tests
