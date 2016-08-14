module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import EDDA.Schema.ParserTest (parserTests)
import EDDA.Schema.CommodityV2Test (commodityV2Tests)
import EDDA.Schema.OutfittingV1Test (outfittingV1Tests)
import EDDA.Schema.ShipyardV1Test (shipyardV1Tests)

tests = hUnitTestToTests $ TestList (parserTests ++ commodityV2Tests ++ outfittingV1Tests ++ shipyardV1Tests) 

main = defaultMain tests
