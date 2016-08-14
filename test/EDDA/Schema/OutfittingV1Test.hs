{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.OutfittingV1Test where

import Test.HUnit

import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Reader

import EDDA.Config
import EDDA.Schema.OutfittingV1 (parseOutfitting)
import EDDA.Schema.Util
import EDDA.Schema.Parser
import EDDA.Types
import EDDA.Test.Util

test1 :: Test
test1 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/outfitting1.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "" (headerUploaderId header)
                      assertEqual "software name" "ED-IBE (API)" (headerSoftwareName header)
                      assertEqual "software version" "0.3.3" (headerSoftwareVersion header)
                      maybeMessage <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeMessage)
                      let outfittingInfo = fromJust maybeMessage
                      assertEqual "system name" "Veroo" (outfittingInfoSystemName outfittingInfo)
                      assertEqual "station name" "Hunziker Terminal" (outfittingInfoStationName outfittingInfo)
                      assertEqual "outfitting modules" 329 (HM.size (outfittingInfoModules outfittingInfo))
                      

outfittingV1Tests = [TestLabel "outfitting1 test" test1]

