{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.CommodityV3Test where

import Test.HUnit

import Data.Maybe (fromJust,isJust)
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Reader

import EDDA.Schema.CommodityV2 (parseCommodity)
import EDDA.Schema.Util
import EDDA.Schema.Parser
import EDDA.Types
import EDDA.Test.Util

test1 :: Test
test1 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/commodity2.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "Kmazz" (headerUploaderId header)
                      assertEqual "software name" "E:D Market Connector [Windows]" (headerSoftwareName header)
                      assertEqual "software version" "2.1.7.1" (headerSoftwareVersion header)
                      maybeCommodity <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeCommodity)
                      let commodityInfo = fromJust maybeCommodity
                      assertEqual "system name" "Carns" (commodityInfoSystemName commodityInfo)
                      assertEqual "station name" "Humason Orbital" (commodityInfoStationName commodityInfo)
                      assertEqual "commodities" 84 (HM.size (commodityInfoCommodities commodityInfo))
                      

commodityV3Tests = [TestLabel "commodity2 test" test1]
