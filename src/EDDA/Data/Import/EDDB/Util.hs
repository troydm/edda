{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Import.EDDB.Util where

import EDDA.Types
import EDDA.Schema.Util (getStr, getStrArray, getStrNullable, getInt, getIntNullable, getDouble, getDoubleNullable)

import qualified Data.Text as T
import qualified Data.ByteString as BC
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Int (Int64(..))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Bson as B
import System.IO (hClose)
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAcceptEncoding)
import Network.HTTP.Client
import Network.HTTP.Client.TLS

mapStr :: Str -> Str -> Value -> Maybe B.Field
mapStr from to obj = do s <- getStr obj from
                        return (to B.:= (B.val s))

mapStrNullable :: Str -> Str -> Value -> Maybe B.Field
mapStrNullable from to obj = do s <- getStrNullable obj from
                                return (to B.:= (if T.length s > 0 then (B.val s) else B.Null))

mapInt :: Str -> Str -> Value -> Maybe B.Field
mapInt from to obj = do !x <- getInt obj from
                        return (to B.:= (B.Int64 (fromIntegral x)))

mapIntNullable :: Str -> Str -> Value -> Maybe B.Field
mapIntNullable from to obj = do !x <- getIntNullable obj from
                                return (to B.:= (B.Int64 (fromIntegral x)))

mapBool :: Str -> Str -> Value -> Maybe B.Field
mapBool from to obj = do !x <- getInt obj from
                         return (to B.:= (B.val (if x == 1 then True else False)))

mapBoolNullable :: Str -> Str -> Value -> Maybe B.Field
mapBoolNullable from to obj = do x <- getIntNullable obj from
                                 return (to B.:= (B.val (if x == 1 then True else False)))

mapDouble :: Str -> Str -> Value -> Maybe B.Field
mapDouble from to obj = do x <- getDouble obj from
                           return (to B.:= (B.val x))

mapDoubleNullable :: Str -> Str -> Value -> Maybe B.Field
mapDoubleNullable from to obj = do x <- getDoubleNullable obj from
                                   return (to B.:= (B.val x))

mapConst :: Str -> B.Value -> Value -> Maybe B.Field
mapConst to val _ = return (to B.:= val)

mapStrArray :: Str -> Str -> Value -> Maybe B.Field
mapStrArray from to obj = case getStrArray obj from of
                                Just sa -> return (to B.:= (B.valList sa))
                                Nothing -> return (to B.:= (B.valList ([] :: [Str])))

mapToDocument :: [Value -> Maybe B.Field] -> Value -> Maybe B.Document
mapToDocument mappers v = allJust $! map (\f -> f $! v) mappers

countJSONDocumentLength :: C.ByteString -> Int -> Int
countJSONDocumentLength d offset = until 0 0
                                   where until c a = let nc = C.index d (offset + a) in
                                              if nc == '}' then until (c-1) (a+1)
                                              else if nc == '{' then until (c+1) (a+1)
                                              else if c == 0 then a 
                                              else until c (a+1)


streamParseIO :: Int -> C.ByteString -> (C.ByteString -> Int -> Int -> IO ()) -> IO ()
streamParseIO bs d f = if C.head d == '[' then until 1 1 0
                       else return ()
                       where until a as ds = 
                                     let na = countJSONDocumentLength d a 
                                         nc = C.index d a
                                     in 
                                     if na == 0 then if nc == ']' then if ds == 0 then return () else f d as (a+1)
                                                     else if nc == ',' then until (a+1) (if ds == 0 then as + 1 else as) ds
                                                     else error "Couldn't stream JSON document"
                                     else if ds+1 == bs then  f d as (a+na+1) >> until (a+na) (a+na) 0
                                          else until (a+na) as (ds+1)

writeToTemp f h msg res = read h >> hClose h >> C.putStrLn msg
                      where br = responseBody res
                            read h = brRead br >>= \s -> if BC.null s then return () else BC.hPut h s >> read h

download url msg f h = 
                   do manager <- newManager tlsManagerSettings
                      initialRequest <- parseRequest url
                      let request = initialRequest { requestHeaders=[(hAcceptEncoding,"gzip, deflate, sdch")] }
                      withResponse request manager (writeToTemp f h msg)

