{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Subscriber where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Trans.Reader
import Control.Concurrent.MVar
import System.Exit
import System.IO
import System.Environment
import System.ZMQ4.Monadic
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy.Char8 as CL
import Codec.Compression.Zlib (decompress)
import System.Log.Logger
import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)

import EDDA.Types
import EDDA.Data.Database (saveError, saveShips, saveCommodities, saveOutfittings)
import EDDA.Schema.Parser (parseHeader, parseMessage)
import EDDA.Schema.Util (showValue)

saveMessage :: Header -> MessageInfo -> ConfigT ()
saveMessage _ IgnoreInfo = return ()

saveMessage _ CommodityInfo {
                        commodityInfoSystemName = systemName,
                        commodityInfoStationName = stationName,
                        commodityInfoTimestamp = timestamp,
                        commodityInfoCommodities = commodities } = do
                                liftIO $ infoM "EDDA.Subscriber" ("saving commodities info " ++ T.unpack systemName ++ " / " ++ T.unpack stationName)
                                saveCommodities systemName stationName (Commodities commodities timestamp)
saveMessage _ OutfittingInfo {
                       outfittingInfoSystemName = systemName,
                       outfittingInfoStationName = stationName,
                       outfittingInfoTimestamp = timestamp,
                       outfittingInfoModules = modules } = do
                                liftIO $ infoM "EDDA.Subscriber" ("saving outfitting info " ++ T.unpack systemName ++ " / " ++ T.unpack stationName)
                                saveOutfittings systemName stationName (Outfittings modules timestamp)
saveMessage _ ShipyardInfo {
                       shipyardInfoSystemName = systemName,
                       shipyardInfoStationName = stationName,
                       shipyardInfoTimestamp = timestamp,
                       shipyardInfoShips = ships } = do
                            liftIO $ infoM "EDDA.Subscriber" ("saving shipyard info " ++ T.unpack systemName ++ " / " ++ T.unpack stationName)
                            saveShips systemName stationName (Ships ships timestamp)

processMessage :: CS.ByteString -> ConfigT ()
processMessage v = let !decompressed = (CL.toStrict . decompress . CL.fromStrict) v in
                   let !result = parseOnly json' decompressed in
                   case result of
                        Right obj -> do header <- parseHeader obj
                                        msg <- parseMessage obj
                                        case header of 
                                            Just h -> case msg of
                                                            Just m -> saveMessage h m
                                                            Nothing -> saveError "Couldn't parse message" (TE.decodeUtf8 decompressed)
                                            Nothing -> saveError "Couldn't parse message" (TE.decodeUtf8 decompressed)
                        Left error -> liftIO $ errorM "EDDA.Subscriber" error

continueRunning mv = do
                       res <- tryTakeMVar mv
                       case res of
                            Just _ -> return False
                            Nothing -> return True

startSubscriber :: MVar () -> MVar () -> ConfigT ()
startSubscriber mv mv2 = do 
              conf <- ask
              liftIO $ infoM "EDDA.Subscriber" "ZeroMQ subscriber started"
              runZMQ $ do
                sub <- socket Sub
                subscribe sub ""
                connect sub (zeroMqHost conf)
                whileM_ (liftIO $ continueRunning mv) $ 
                          do msg <- receive sub
                             liftIO $ forkIO (runReaderT (processMessage msg) conf)
              liftIO $ infoM "EDDA.Subscriber" "ZeroMQ subscriber stopped"
              liftIO $ putMVar mv2 ()
              return ()
