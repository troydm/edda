{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.OutfittingV2Test where

import Test.HUnit

import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.Reader

import EDDA.Config
import EDDA.Schema.OutfittingV2 (parseOutfitting)
import EDDA.Schema.Util
import EDDA.Schema.Parser
import EDDA.Types
import EDDA.Test.Util

test1 :: Test
test1 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/outfitting3.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "3b13d63ce38f852c5b012b9868ccf805" (headerUploaderId header)
                      assertEqual "software name" "E:D Market Connector [Windows]" (headerSoftwareName header)
                      assertEqual "software version" "2.1.7.1" (headerSoftwareVersion header)
                      maybeMessage <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeMessage)
                      let outfittingInfo = fromJust maybeMessage
                      assertEqual "system name" "Guguroro" (outfittingInfoSystemName outfittingInfo)
                      assertEqual "station name" "Goodricke Enterprise" (outfittingInfoStationName outfittingInfo)
                      assertEqual "outfitting modules" 232 (HM.size (outfittingInfoModules outfittingInfo))
                      
test2 :: Test
test2 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/outfitting4.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "3b13d63ce38f852c5b012b9868ccf805" (headerUploaderId header)
                      assertEqual "software name" "E:D Market Connector [Windows]" (headerSoftwareName header)
                      assertEqual "software version" "2.1.7.1" (headerSoftwareVersion header)
                      maybeMessage <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeMessage)
                      let outfittingInfo = fromJust maybeMessage
                      assertEqual "system name" "Baramat" (outfittingInfoSystemName outfittingInfo)
                      assertEqual "station name" "Merritt Platform" (outfittingInfoStationName outfittingInfo)
                      assertEqual "outfitting modules" 101 (HM.size (outfittingInfoModules outfittingInfo))

test3 :: Test
test3 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/outfitting5.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "617637dfa49d5a9a49a48b99c6be8317c21eefd9" (headerUploaderId header)
                      assertEqual "software name" "E:D Market Connector [Windows]" (headerSoftwareName header)
                      assertEqual "software version" "3.4.3.0" (headerSoftwareVersion header)
                      maybeMessage <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeMessage)
                      let outfittingInfo = fromJust maybeMessage
                      assertEqual "system name" "Tir" (outfittingInfoSystemName outfittingInfo)
                      assertEqual "station name" "The Watchtower" (outfittingInfoStationName outfittingInfo)
                      assertEqual "outfitting modules" 32 (HM.size (outfittingInfoModules outfittingInfo))

outfittingV2Tests = [TestLabel "outfitting3 test" test1, TestLabel "outfitting4 test" test2, TestLabel "outfitting5 test" test3]

