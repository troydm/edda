{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module EDDA.Data.Cache (Cache,newCache,cachedIO,cachedForkIO) where

import Data.Maybe (isJust)
import Data.Int (Int64(..))
import Data.Time
import Data.Time.Clock.POSIX
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

type Cache a = MVar (Int64, Maybe (a,Int64))

getCurrTime :: IO Int64
getCurrTime = round `fmap` getPOSIXTime

newCache :: Int64 -> IO (Cache a)
newCache t = newMVar (t, Nothing)

putValueInCache c t a = do !v' <- a
                           !currTime <- getCurrTime
                           putMVar c (t, Just (v',currTime))
                           return v'

cachedIO :: Cache a -> IO a -> IO a
cachedIO c a = do
                  (!t, !v) <- takeMVar c
                  if isJust v then do 
                                   let Just (!c',!l) = v
                                   !currTime <- getCurrTime
                                   if (currTime - l) >= t then putValueInCache c t a
                                   else putMVar c (t, v) >> return c'
                  else putValueInCache c t a

cachedForkIO :: Cache a -> IO a -> IO a
cachedForkIO c a = do
                  (!t, !v) <- takeMVar c
                  if isJust v then do 
                                   let Just (!c',!l) = v
                                   !currTime <- getCurrTime
                                   if (currTime - l) >= t then do !currTime <- getCurrTime
                                                                  putMVar c (t, Just (c',currTime))
                                                                  forkIO (do !v' <- a
                                                                             !currTime <- getCurrTime
                                                                             (!t, !v) <- takeMVar c
                                                                             putMVar c (t, Just (v',currTime)) )
                                                                  return c'
                                   else putMVar c (t, v) >> return c'
                  else putValueInCache c t a
