{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.Util where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad (join)

import Data.Maybe (catMaybes)
import Data.Aeson
import Data.Aeson.Types
import EDDA.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Data.Time.ISO8601 (parseISO8601) 

showValue :: Value -> ConfigT ()
showValue = lift . C.putStrLn . C.pack . show

getTimestamp :: Value -> Str -> Maybe Timestamp
getTimestamp (Object v) s = case HM.lookup (toText s) v of
                                    Just (String t) -> let dt = T.unpack t in
                                                       case parseISO8601 dt of
                                                            Just dt -> Just dt
                                                            Nothing -> parseISO8601 (dt ++ "Z")
                                    Nothing -> Nothing
getTimestamp _ _ = Nothing

valToStr :: Value -> Maybe Str
valToStr (String s) = Just $ TE.encodeUtf8 s
valToStr _ = Nothing

valToStrNullable :: Value -> Maybe Str
valToStrNullable Null = Just ""
valToStrNullable v = valToStr v

valToInt :: Value -> Maybe Int
valToInt v = case fromJSON v :: Result Int of
                Success v -> Just v
                Error _ -> Nothing

valToIntNullable :: Value -> Maybe Int
valToIntNullable v = case fromJSON v :: Result Int of
                        Success v -> Just v
                        Error _ -> Just 0

valToDouble :: Value -> Maybe Double
valToDouble v = case fromJSON v :: Result Double of
                    Success v -> Just v
                    Error _ -> Nothing

valToDoubleNullable :: Value -> Maybe Double
valToDoubleNullable v = case fromJSON v :: Result Double of
                            Success v -> Just v
                            Error _ -> Just 0.0

getStr :: Value -> Str -> Maybe Str
getStr (Object v) s =  case (join $ valToStr <$> HM.lookup (toText s) v) of
                         Just s -> if C.length s > 0 then Just s else Nothing
                         Nothing -> Nothing
getStr _ _ = Nothing

getStrNullable :: Value -> Str -> Maybe Str
getStrNullable (Object v) s =  case (join $ valToStrNullable <$> HM.lookup (toText s) v) of
                                    Just s -> Just s
                                    Nothing -> Nothing
getStrNullable _ _ = Nothing

getChr :: Value -> Str -> Maybe Char
getChr v s = C.head <$> getStr v s

getInt :: Value -> Str -> Maybe Int
getInt (Object v) s = join $ valToInt <$> HM.lookup (toText s) v
getInt _ _ = Nothing

getIntNullable :: Value -> Str -> Maybe Int
getIntNullable (Object v) s = join $ valToIntNullable <$> HM.lookup (toText s) v
getIntNullable _ _ = Nothing

getDouble :: Value -> Str -> Maybe Double
getDouble (Object v) s = join $ valToDouble <$> HM.lookup (toText s) v
getDouble _ _ = Nothing

getDoubleNullable :: Value -> Str -> Maybe Double
getDoubleNullable (Object v) s = join $ valToDoubleNullable <$> HM.lookup (toText s) v
getDoubleNullable _ _ = Nothing

getLevel :: Value -> Str -> Maybe Level
getLevel v s = case getStr v s of
                 Just s -> if s == "Low" then Just Low
                           else if s == "Med" then Just Med
                           else if s == "High" then Just High
                           else Nothing
                 Nothing -> Nothing

getMount :: Value -> Str -> Maybe Mount
getMount v s = case getStr v s of
                 Just s -> if s == "Fixed" then Just Fixed
                           else if s == "Gimballed" then Just Gimballed
                           else if s == "Gimbal" then Just Gimballed
                           else if s == "Turreted" then Just Turreted
                           else if s == "Turret" then Just Turreted
                           else Nothing
                 Nothing -> Nothing

getGuidance :: Value -> Str -> Maybe Guidance
getGuidance v s = case getStr v s of
                 Just s -> if s == "Dumbfire" then Just Dumbfire
                           else if s == "Seeker" then Just Seeker
                           else Nothing
                 Nothing -> Nothing

schemaRef :: Value -> Maybe Str
schemaRef v = getStr v "$schemaRef"

message (Object v) = HM.lookup "message" v
message _ = Nothing

getArray :: Value -> Str -> Maybe [Value]
getArray (Object v) s = case HM.lookup (toText s) v of
                             Just (Array a) -> Just $ V.toList a
                             Just _ -> Nothing
                             Nothing -> Nothing
getArray _ _ = Nothing


getStrArray :: Value -> Str -> Maybe [Str]
getStrArray v s = case getArray v s of
                    Just a -> let sl = map valToStr a in
                              Just $ catMaybes sl
                    Nothing -> Nothing


getIntArray :: Value -> Str -> Maybe [Int]
getIntArray v s = case getArray v s of
                    Just a -> let sl = map valToInt a in
                              Just $ catMaybes sl
                    Nothing -> Nothing
