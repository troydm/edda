{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

{-
  detachDaemon and related functions are copied from John Goerzen's MissingH package and modified for use with edda, see the copyright below
-}

{-
  Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>
 
  All rights reserved.
 
  For license and copyright information, see the file LICENSE
  Module     : System.Daemon
  Copyright  : Copyright (C) 2005-2011 John Goerzen
  License    : BSD3
  Tools for writing daemons\/server processes

  Written by John Goerzen, jgoerzen\@complete.org

  Please note: Most of this module is not compatible with Hugs.

  Messages from this module are logged under @System.Daemon@.  See
  'System.Log.Logger' for details.

  Based on background
  from <http://www.erlenstar.demon.co.uk/unix/faq_2.html#SEC16> and
  <http://www.haskell.org/hawiki/HaskellUnixDaemon>.

  This module is not available on Windows.
-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Posix.Signals
import System.IO
import System.Console.CmdArgs
import System.Posix.Process
import System.Posix.IO
import System.Directory
import System.Log.Logger
import System.Exit
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Formatter (simpleLogFormatter)
import System.Process (callCommand)


import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C

import EDDA.Types
import EDDA.Config
import EDDA.Data.Query
import EDDA.Data.Subscriber
import EDDA.Data.Import.EDDB.Systems (downloadAndImport)
import EDDA.Data.Import.EDDB.Stations (downloadAndImport)

trap :: IO a -> IO a
trap = traplogging "System.Daemon" ERROR "detachDaemon"

ppMaybeList :: Maybe [Str] -> IO ()
ppMaybeList (Just l) = mapM_ C.putStrLn l
ppMaybeList (Nothing) = return ()

detachDaemon :: IO () -> IO ()
detachDaemon act = trap $ do forkProcess (child1 act)
                             exitImmediately ExitSuccess

child1 :: IO () -> IO ()
child1 act = trap $ do createSession
                       forkProcess (child2 act)
                       exitImmediately ExitSuccess

child2 :: IO () -> IO ()
child2 act = do trap $ do setCurrentDirectory "/"
                          mapM_ closeFd [stdInput, stdOutput, stdError]
                          nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
                          mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
                          closeFd nullFd
                act

setRootLogger :: Config -> IO ()
setRootLogger conf = do handler <- fileHandler (logPath conf) DEBUG >>= 
                            \lh -> return $ setFormatter lh (simpleLogFormatter "$prio:$loggername:$time - $msg")
                        updateGlobalLogger rootLoggerName (addHandler handler)
                        updateGlobalLogger rootLoggerName (setLevel INFO)

termHandler :: MVar () -> Handler
termHandler mv = CatchOnce $ do
    putMVar mv ()

startSubscriber :: Config -> IO ()
startSubscriber conf = 
    do
        mv <- newEmptyMVar
        mv2 <- newEmptyMVar
        installHandler sigTERM (termHandler mv) Nothing
        infoM "EDDA.Subscriber" "Starting ZeroMQ subscriber"
        forkIO $ runReaderT (EDDA.Data.Subscriber.startSubscriber mv2 mv) conf
        takeMVar mv
        infoM "EDDA.Subscriber" "Stopping ZeroMQ subscriber"
        putMVar mv2 ()
        takeMVar mv


data Command = StartCommand { conf :: String, foreground :: Bool } 
             | StopCommand { conf :: String } 
             | ImportCommand { conf :: String, source :: String, target :: String }
             | SystemsWithinLy { conf :: String, system :: String, distance :: Double } deriving (Data,Typeable,Show,Eq)

startCommand = StartCommand { conf = "edda.conf" &= name "config" &= name "c" &= opt ("edda.conf" :: String) &= help "config path", 
                              foreground = def &= name "foreground" &= name "f" &= help "run in foreground"} &= name "start" &= help "start edda daemon"

stopCommand = StopCommand { conf = "edda.conf" &= name "config" &= name "c" &= opt ("edda.conf" :: String) &= help "config path" } &= name "stop" &= help "stop edda daemon"

importCommand = ImportCommand { source = def &= name "source" &= help "source system such as eddb", 
                                target = def &= name "target" &= help "target to import",
                                conf = "edda.conf" &= name "config" &= name "c" &= opt ("edda.conf" :: String) &= help "config path" } &= name "import" &= help "import data"

systemsWithinLy = SystemsWithinLy { conf = "edda.conf" &= name "config" &= name "c" &= opt ("edda.conf" :: String) &= help "config path",  
                                    system = def &= name "system" &= name "s", distance = def &= name "distance" &= name "d" } &= name "systemsWithinLy" &= help "systems within ly"

mode = modes [startCommand,stopCommand,importCommand,systemsWithinLy] &= help "edda - help" &= program "edda" &= summary "edda v0.1\nElite Dangerous Data Aggregator"

main :: IO ()
main = do
         cmd <- cmdArgs mode 
         case cmd of 
            ImportCommand { conf = configPath, source = source, target = target } -> 
                                                                               do conf <- readConfig configPath
                                                                                  let s = fmap toLower source
                                                                                  let t = fmap toLower target 
                                                                                  if s == "eddb" then
                                                                                      if t == "systems" then runReaderT EDDA.Data.Import.EDDB.Systems.downloadAndImport conf
                                                                                      else if t == "stations" then runReaderT EDDA.Data.Import.EDDB.Stations.downloadAndImport conf
                                                                                      else putStrLn "Invalid target, currently supported targets are: [systems,stations]"
                                                                                  else putStrLn "Invalid source, currently supported sources are: [eddb]"
            StartCommand { conf = configPath, foreground = foreground } -> do conf <- readConfig configPath
                                                                              if foreground then setRootLogger conf >> Main.startSubscriber conf
                                                                              else detachDaemon (setRootLogger conf >> Main.startSubscriber conf)
            StopCommand { conf = configPath} -> do conf <- readConfig configPath
                                                   callCommand "pkill edda" >> return ()
            SystemsWithinLy { conf = configPath, system = system, distance = distance } -> 
                                do conf <- readConfig configPath
                                   runReaderT (getSystemsWithinLyFrom (C.pack system) distance >>= liftIO . ppMaybeList ) conf
