{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Import.EDDB.Systems where

import EDDA.Types
import EDDA.Data.Database (saveSystems)
import EDDA.Schema.Util (getStr, getInt, getDouble)
import EDDA.Data.Import.EDDB.Util

import Data.IORef
import qualified Data.Vector as V
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Concurrent

import System.IO.Temp (withSystemTempFile)

import qualified Data.Text as T
import qualified Data.ByteString as BC
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int32(..))
import qualified Data.Bson as B

url = "https://eddb.io/archive/v4/systems.json"

toDocument :: Value -> Maybe (Int32,Str,B.Document)
toDocument obj = do edsmId <- fromIntegral <$> getInt obj "edsm_id"
                    name <- getStr obj "name"
                    doc <- mapToDocument [ mapInt "id" "eddbId",
                                           mapConst "edsmId" (B.val edsmId),
                                           mapConst "systemName" (B.val (toText name)),
                                           mapStrNullable "security" "security",
                                           mapStrNullable "state" "state",
                                           mapStrNullable "government" "government",
                                           mapStrNullable "faction" "faction",
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
 

saveToDatabase systems = do liftIO $ putStrLn "Importing into database..."
                            saveSystems systems
                            liftIO $ putStrLn ("Systems imported: " ++ (show (length systems)))


convertAndSaveToDB :: Config -> C.ByteString -> IO ()
convertAndSaveToDB c d = do total <- newIORef 0 
                            streamParseIO 10000 d (saveToDB total)
                            totalCount <- readIORef total
                            putStrLn ("Total systems imported: " ++ (show totalCount))
           where substr d s e = C.concat ["[",(C.take (e-s-1) $ C.drop s d),"]"]
                 convert s = case (decodeStrict' s :: Maybe Value) of
                                Just (Array systems) -> runReaderT (toDocumentList systems) c >>= return . Just. onlyJustVec
                                Just _ -> return Nothing
                                Nothing -> return Nothing
                 saveToDB total d s e = do maybeSystems <- (convert (substr d s e))
                                           case maybeSystems of
                                                Just systems -> runReaderT (saveToDatabase (V.toList systems)) c >> let totalCount = length systems in totalCount `seq` modifyIORef' total (+ totalCount)
                                                Nothing -> putStrLn "Couldn't decode a batch" >> C.putStrLn (substr d s e)

downloadAndImport :: ConfigT ()
downloadAndImport = 
            do
             liftIO $ C.putStrLn "Downloading EDDB Systems data..."
             r <- ask
             liftIO $ withSystemTempFile "systems.json" (\f h -> download url "EDDB Systems data downloaded" f h >> C.readFile f >>= convertAndSaveToDB r)

