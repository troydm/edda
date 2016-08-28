{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.ShipyardV2Test where

import Test.HUnit

import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.HashSet as HS
import Control.Monad.Trans.Reader

import EDDA.Config
import EDDA.Schema.OutfittingV1 (parseOutfitting)
import EDDA.Schema.Util
import EDDA.Schema.Parser
import EDDA.Types
import EDDA.Test.Util

test1 :: Test
test1 = TestCase $ do conf <- readConf
                      val <- readJsonTestFile "test/EDDA/Schema/shipyard2.json"
                      maybeHeader <- runReaderT (parseHeader val) conf
                      assertBool "header couldn't be parsed" (isJust maybeHeader)
                      let header = fromJust maybeHeader
                      assertEqual "uploader id" "Dovacube" (headerUploaderId header)
                      assertEqual "software name" "E:D Market Connector [Windows]" (headerSoftwareName header)
                      assertEqual "software version" "2.1.7.0" (headerSoftwareVersion header)
                      maybeMessage <- runReaderT (parseMessage val) conf
                      assertBool "message couldn't be parsed" (isJust maybeMessage)
                      let shipyardInfo = fromJust maybeMessage
                      assertEqual "system name" "Gateway" (shipyardInfoSystemName shipyardInfo)
                      assertEqual "station name" "Dublin Citadel" (shipyardInfoStationName shipyardInfo)
                      assertEqual "ships" 3 (HS.size (shipyardInfoShips shipyardInfo))
                      

shipyardV2Tests = [TestLabel "shipyard2 test" test1]

