module EDDA.Test.Util where

import Paths_edda
import Data.Maybe (fromJust,isJust)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as C
import System.Log.Logger
import System.Log.Handler.Simple (streamHandler)
import System.IO (stdout)

import EDDA.Config

setRootLogger :: IO ()
setRootLogger = do handler <- streamHandler stdout DEBUG
                   updateGlobalLogger rootLoggerName (addHandler handler)
                   updateGlobalLogger rootLoggerName (setLevel INFO)

readConf = do c <- getDataFileName "edda.conf" >>= readConfig
              setRootLogger 
              return c

readJsonTestFile file = getDataFileName file >>= C.readFile >>= return . fromJust . decodeStrict'

