module EDDA.Test.Util where

import Paths_edda
import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C

import EDDA.Config

readConf = getDataFileName "edda.conf" >>= readConfig

readJsonTestFile file = getDataFileName file >>= C.readFile >>= return . fromJust . decodeStrict'

