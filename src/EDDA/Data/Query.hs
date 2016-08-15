{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Query where

import qualified Data.Vector as V
import qualified Data.Text as T

import EDDA.Types
import EDDA.Data.Database

distanceBetween s1 s2 = let x' = ((systemCoordX s1) - (systemCoordX s2)) in
                                  let y' = ((systemCoordY s1) - (systemCoordY s2)) in
                                  let z' = ((systemCoordZ s1) - (systemCoordZ s2)) in
                                  sqrt ((x'*x')+(y'*y')+(z'*z'))

getSystemsWithinLyFrom :: Str -> Double -> ConfigT (Maybe [T.Text])
getSystemsWithinLyFrom systemName distance = 
    do maybeSystemCoord <- getSystemCoord systemName
       case maybeSystemCoord of
        Just systemCoord -> do maybeSystemCoords <- getSystemCoords (\s -> distanceBetween systemCoord s <= distance) 
                               return $ do systemCoords <- maybeSystemCoords
                                           return $ map systemCoordSystemName systemCoords
        Nothing -> return Nothing


getSystemsWithinLyFrom' :: Str -> Double -> V.Vector SystemCoord -> ConfigT (Maybe [T.Text])
getSystemsWithinLyFrom' systemName distance allSystemCoords = 
    do maybeSystemCoord <- getSystemCoord systemName
       case maybeSystemCoord of
        Just systemCoord -> do let systemCoords = V.filter (\s -> distanceBetween systemCoord s <= distance) allSystemCoords
                               return $ Just (V.toList (V.map systemCoordSystemName systemCoords))
        Nothing -> return Nothing
