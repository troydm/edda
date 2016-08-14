module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import EDDA.Schema.CommodityV2Test (commodityV2Tests)

tests = hUnitTestToTests $ TestList commodityV2Tests

main = defaultMain tests
