{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Import.EDDB.Systems where

import EDDA.Types
import EDDA.Data.Database (query, saveSystems)
import EDDA.Schema.Util (getStr, getInt, getDouble)
import EDDA.Data.Import.EDDB.Util

import Data.IORef
import qualified Data.Vector as V
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Concurrent
import Control.Applicative ((<|>))

import System.IO.Temp (withSystemTempFile)
import System.IO.MMap (mmapFileByteString)

import qualified Data.Text as T
import qualified Data.ByteString as BC
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int32(..))
import qualified Data.Bson as B

url = "https://eddb.io/archive/v6/systems_populated.json"

toDocument :: Value -> Maybe (Int32,Str,B.Document)
toDocument obj = do !edsmId <- fromIntegral <$> ((getInt obj "edsm_id") <|> (Just 0 :: Maybe Int))
                    !name <- getStr obj "name"
                    !doc <- mapToDocument [mapIntNullable "id" "eddbId",
                                           mapIntNullable "edsm_id" "edsmId",
                                           mapConst "systemName" (B.val (toText name)),
                                           mapStrNullable "security" "security",
                                           mapObjectArray "states" "states" (
                                            mapToDocument [mapIntNullable "id" "id",
                                                           mapStrNullable "name" "name"]
                                           ),
                                           mapStrNullable "government" "government",
                                           mapIntNullable "controlling_minor_faction_id" "factionId",
                                           mapStrNullable "controlling_minor_faction" "faction",
                                           mapStrNullable "allegiance" "allegiance",
                                           mapDoubleNullable "population" "population",
                                           mapStrNullable "power" "power",
                                           mapStrNullable "power_state" "powerState",
                                           mapBoolNullable "is_populated" "isPopulated",
                                           mapStrNullable "primary_economy" "primaryEconomy",
                                           mapBoolNullable "needs_permit" "needsPermit",
                                           mapDouble "x" "x",
                                           mapDouble "y" "y",
                                           mapDouble "z" "z" ] obj
                    return (edsmId,name,doc)

toDocumentList :: V.Vector Value -> ConfigT (V.Vector (Maybe (Int32,Str,B.Document)))
toDocumentList systems = flip V.mapM systems (\v -> case toDocument v of
                                                        Just d -> return $ Just d
                                                        Nothing -> do liftIO $ C.putStrLn "Couldn't parse system: "
                                                                      liftIO $ putStrLn (show v)
                                                                      return Nothing)


saveToDatabase systems = do liftIO $ C.putStrLn "Importing into database..."
                            saveSystems systems
                            liftIO $ C.putStrLn ("Systems imported: " `C.append` (C.pack (show (length systems))))


convertAndSaveToDB :: Config -> C.ByteString -> IO ()
convertAndSaveToDB c d = do total <- newIORef 0
                            runReaderT (query (do context <- ask
                                                  (liftIO (streamParseIO 10000 d (saveToDB context total))))) c
                            totalCount <- readIORef total
                            C.putStrLn ("Total systems imported: " `C.append` (C.pack (show totalCount)))
           where substr d s e = C.concat ["[",(C.take (e-s-1) $ C.drop s d),"]"]
                 convert s = case (decodeStrict' s :: Maybe Value) of
                                Just (Array systems) -> runReaderT (toDocumentList systems) c >>= return . Just. onlyJustVec
                                Just _ -> return Nothing
                                Nothing -> return Nothing
                 saveToDB context total d s e =
                                        do maybeSystems <- (convert (substr d s e))
                                           case maybeSystems of
                                                Just systems -> do runReaderT (saveToDatabase (V.toList systems)) context
                                                                   let !totalCount = length systems in modifyIORef' total (+ totalCount)
                                                Nothing -> putStrLn "Couldn't decode a batch" >> C.putStrLn (substr d s e)

downloadAndImport :: ConfigT ()
downloadAndImport = 
            do
             liftIO $ C.putStrLn "Downloading EDDB Systems data..."
             r <- ask
             liftIO $ withSystemTempFile "systems.json" (\f h -> download url "EDDB Systems data downloaded" f h >> mmapFileByteString f Nothing >>= convertAndSaveToDB r)

