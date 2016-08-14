{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.ParserTest where

import Test.HUnit

import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Reader
import Data.Time.Format (formatTime)

import EDDA.Schema.CommodityV2 (parseCommodity)
import EDDA.Schema.Util
import EDDA.Schema.Parser
import EDDA.Types
import EDDA.Test.Util

test1 :: Test
test1 = TestCase $ do let timestamp = "{ \"timestamp\" : \"2016-08-11T20:06:54-05:00\" }"
                      let v = fromJust $ (decodeStrict' timestamp :: Maybe Value)
                      let d = getTimestamp v "timestamp"
                      assertBool "timestamp not parsed" (isJust d)
                      let dt = fromJust d
                      assertEqual "date" "2016-08-12 01:06:54 UTC" (show dt)

test2 :: Test
test2 = TestCase $ do let timestamp = "{ \"timestamp\" : \"2016-08-11T20:6:54-05:00\" }"
                      let v = fromJust $ (decodeStrict' timestamp :: Maybe Value)
                      let d = getTimestamp v "timestamp"
                      assertBool "timestamp not parsed" (isJust d)
                      let dt = fromJust d
                      assertEqual "date" "2016-08-12 01:06:54 UTC" (show dt)

parserTests = [TestLabel "parser timestamp 1 test" test1,
               TestLabel "parser timestamp 2 test" test2]

