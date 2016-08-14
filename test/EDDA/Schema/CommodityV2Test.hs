{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.CommodityV2Test where

import Test.HUnit
import Paths_edda

import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Reader

import EDDA.Config
import EDDA.Schema.CommodityV2 (parseCommodity)
import EDDA.Schema.Util
import EDDA.Types

readConf = getDataFileName "edda.conf" >>= readConfig
readJsonTestFile file = getDataFileName file >>= C.readFile >>= return . fromJust . decodeStrict'

test1 :: Test
test1 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/commodity1.json"
                      parsed <- runReaderT (parseCommodity (fromJust $ message val)) conf
                      assertBool "message couldn't be parsed" (isJust parsed)
                      let commodityInfo = fromJust parsed
                      assertEqual "system name" "Ross 765" (commodityInfoSystemName commodityInfo)
                      assertEqual "station name" "Pogue Platform" (commodityInfoStationName commodityInfo)
                      assertEqual "commodities" 73 (HM.size (commodityInfoCommodities commodityInfo))
                      putStrLn (show commodityInfo)
                      

commodityV2Tests = [TestLabel "commodity1 test" test1]
