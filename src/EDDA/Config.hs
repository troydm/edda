module EDDA.Config (readConfig) where

import Data.ConfigFile
import EDDA.Types
import qualified Data.Text as T
import System.Directory

readConfigProperty :: ConfigParser -> String -> String
readConfigProperty cp name = let val = get cp "DEFAULT" name in
                             case val of
                                Right p -> p
                                Left _ -> error ("Couldn't read config property: " ++ name) 


readConfig :: String -> IO Config
readConfig filename = do val <- readfile (emptyCP { optionxform = id }) filename
                         case val of
                            Right cp -> do let zeroMqHost = readConfigProperty cp "zeroMQHost"
                                           let mongoHost = readConfigProperty cp "mongoHost"
                                           let mongoDb = readConfigProperty cp "mongoDB"
                                           let logPath = readConfigProperty cp "logPath"
                                           absLogPath <- canonicalizePath logPath
                                           return Config { zeroMqHost = zeroMqHost, logPath = absLogPath, mongoHost = mongoHost, mongoDb = T.pack mongoDb }
                            Left _ -> error ("Couldn't read config file: " ++ filename)


