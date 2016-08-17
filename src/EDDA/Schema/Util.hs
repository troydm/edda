{-# LANGUAGE OverloadedStrings #-}
module EDDA.Schema.Util where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad (join)

import Data.Maybe (catMaybes,listToMaybe)
import Data.Aeson
import Data.Aeson.Types
import EDDA.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.HashMap.Strict as HM
import Data.Time.ISO8601 (parseISO8601) 
import Data.List (intercalate)
import Data.List.Split (splitOn,splitOneOf)

showValue :: Value -> ConfigT ()
showValue = lift . C.putStrLn . C.pack . show

getTimestamp :: Value -> Str -> Maybe Timestamp
getTimestamp (Object v) s = case HM.lookup (toText s) v of
                                    Just (String t) -> let dt = T.unpack t in
                                                       orMaybe [parseISO8601 dt, parseISO8601 (dt ++ "Z"), parseISO8601 (fixTimestamp dt)]
                                    Nothing -> Nothing
                            where fixTimestamp dt = let s = splitOn "T" dt
                                                        d = head s
                                                        t = head (tail s) 
                                                        ts = splitOneOf "+-" t
                                                        tm = head ts
                                                        tz = head (tail ts) 
                                                        tms = splitOn ":" tm
                                                        tms' = map (\t -> if length t == 1 then '0':t else t) tms 
                                                        tz' = if elem '+' t then '+':tz else '-':tz in
                                                    d ++ "T" ++ (intercalate ":" tms') ++ tz'
getTimestamp _ _ = Nothing

orMaybe = listToMaybe . catMaybes

valToStr :: Value -> Maybe Str
valToStr (String s) = Just $ TE.encodeUtf8 s
valToStr _ = Nothing

valToText :: Value -> Maybe T.Text
valToText (String s) = Just s
valToText _ = Nothing

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

getText :: Value -> T.Text -> Maybe T.Text
getText (Object v) s =  case HM.lookup s v of
                         Just (String s) -> if T.length s > 0 then Just s else Nothing
                         _ -> Nothing
getText _ _ = Nothing

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

getTextNullable :: Value -> T.Text -> Maybe T.Text
getTextNullable (Object v) s =  case HM.lookup s v of
                                     Just Null -> Just (T.empty)
                                     Just (String s) -> Just s
                                     Nothing -> Nothing
getTextNullable _ _ = Nothing

getChr :: Value -> Str -> Maybe Char
getChr v s = C.head <$> getStr v s

getInt :: Value -> Str -> Maybe Int
getInt (Object v) s = join $ valToInt <$> HM.lookup (toText s) v
getInt _ _ = Nothing

getIntText :: Value -> T.Text -> Maybe Int
getIntText (Object v) s = join $ valToInt <$> HM.lookup s v
getIntText _ _ = Nothing

getIntNullable :: Value -> Str -> Maybe Int
getIntNullable (Object v) s = join $ valToIntNullable <$> HM.lookup (toText s) v
getIntNullable _ _ = Nothing

getIntNullableText :: Value -> T.Text -> Maybe Int
getIntNullableText (Object v) s = join $ valToIntNullable <$> HM.lookup s v
getIntNullableText _ _ = Nothing

getDouble :: Value -> Str -> Maybe Double
getDouble (Object v) s = join $ valToDouble <$> HM.lookup (toText s) v
getDouble _ _ = Nothing

getDoubleText :: Value -> T.Text -> Maybe Double
getDoubleText (Object v) s = join $ valToDouble <$> HM.lookup s v
getDoubleText _ _ = Nothing

getDoubleNullable :: Value -> Str -> Maybe Double
getDoubleNullable (Object v) s = join $ valToDoubleNullable <$> HM.lookup (toText s) v
getDoubleNullable _ _ = Nothing

getDoubleNullableText :: Value -> T.Text -> Maybe Double
getDoubleNullableText (Object v) s = join $ valToDoubleNullable <$> HM.lookup s v
getDoubleNullableText _ _ = Nothing

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

getArrayText :: Value -> T.Text -> Maybe [Value]
getArrayText (Object v) s = case HM.lookup s v of
                             Just (Array a) -> Just $ V.toList a
                             Just _ -> Nothing
                             Nothing -> Nothing
getArrayText _ _ = Nothing


getStrArray :: Value -> Str -> Maybe [Str]
getStrArray v s = case getArray v s of
                    Just a -> let sl = map valToStr a in
                              Just $ catMaybes sl
                    Nothing -> Nothing

getTextArray :: Value -> T.Text -> Maybe [T.Text]
getTextArray v s = case getArrayText v s of
                    Just a -> let sl = map valToText a in
                              Just $ catMaybes sl
                    Nothing -> Nothing


getIntArray :: Value -> Str -> Maybe [Int]
getIntArray v s = case getArray v s of
                    Just a -> let sl = map valToInt a in
                              Just $ catMaybes sl
                    Nothing -> Nothing
