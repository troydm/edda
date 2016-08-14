{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.CommodityV2Test where

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
                      val <- readJsonTestFile "test/EDDA/Schema/commodity1.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "JimKeir" (headerUploaderId header)
                      assertEqual "software name" "EVA [Android]" (headerSoftwareName header)
                      assertEqual "software version" "1.0" (headerSoftwareVersion header)
                      maybeCommodity <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeCommodity)
                      let commodityInfo = fromJust maybeCommodity
                      assertEqual "system name" "Ross 765" (commodityInfoSystemName commodityInfo)
                      assertEqual "station name" "Pogue Platform" (commodityInfoStationName commodityInfo)
                      assertEqual "commodities" 73 (HM.size (commodityInfoCommodities commodityInfo))
                      

commodityV2Tests = [TestLabel "commodity1 test" test1]
