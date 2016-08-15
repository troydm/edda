{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Cache (Cache,newCache,cachedIO) where

import Data.Maybe (isJust)
import Data.Int (Int64(..))
import Data.Time
import Data.Time.Clock.POSIX
import Control.Concurrent.MVar

type Cache a = MVar (Int64, Maybe (a,Int64))

getCurrTime :: IO Int64
getCurrTime = round `fmap` getPOSIXTime

newCache :: Int64 -> IO (Cache a)
newCache t = newMVar (t, Nothing)

cachedIO :: Cache a -> IO a -> IO a
cachedIO c a = do
                  (!t, !v) <- takeMVar c
                  if isJust v then do 
                                   let Just (!c',!l) = v
                                   !currTime <- getCurrTime
                                   if (currTime - l) >= t then do !v' <- a
                                                                  !currTime <- getCurrTime
                                                                  putMVar c (t, Just (v',currTime))
                                                                  return v'
                                   else putMVar c (t, v) >> return c'
                  else do !v' <- a
                          !currTime <- getCurrTime
                          putMVar c (t, Just (v',currTime))
                          return v'