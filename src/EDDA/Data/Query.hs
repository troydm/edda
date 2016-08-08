{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Query where

import EDDA.Types
import EDDA.Data.Database


getSystemsWithinLyFrom :: Str -> Double -> ConfigT (Maybe [Str])
getSystemsWithinLyFrom systemName distance = 
    do maybeSystemCoord <- getSystemCoord systemName
       case maybeSystemCoord of
        Just systemCoord -> do maybeSystemCoords <- getSystemCoords (\s -> distanceBetween systemCoord s <= distance) 
                               return $ do systemCoords <- maybeSystemCoords
                                           return $ map systemCoordSystemName systemCoords
        Nothing -> return Nothing
    where distanceBetween s1 s2 = let x' = ((systemCoordX s1) - (systemCoordX s2)) in
                                  let y' = ((systemCoordY s1) - (systemCoordY s2)) in
                                  let z' = ((systemCoordZ s1) - (systemCoordZ s2)) in
                                  sqrt ((x'*x')+(y'*y')+(z'*z'))
     


