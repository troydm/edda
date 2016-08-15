{-# LANGUAGE OverloadedStrings #-}
module EDDA.Data.Rest where

import Web.Scotty
import Network.Wai.Middleware.Gzip

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import System.Log.Logger
import Data.Aeson.Encode.Pretty (encodePretty)

import EDDA.Types
import EDDA.Data.Database (getAllSystemCoords)
import EDDA.Data.Cache
import EDDA.Data.Query (getSystemsWithinLyFrom')

jsonHeader = setHeader "content-type" "text/json"

jsonOut val = jsonHeader >> raw (encodePretty val)

servicePath path s = capture $ path ++ s

systemsWithinLy cache c path = get (servicePath path "/systemsWithinLy/:systemName/:distance") $ do
                            allSystemCoords <- liftIO (cachedForkIO cache (runReaderT getAllSystemCoords c))
                            systemName <- param "systemName"
                            distance <- param "distance" :: ActionM Double
                            maybeSystems <- liftIO (runReaderT (getSystemsWithinLyFrom' systemName distance allSystemCoords) c)
                            let systems = maybe ([] :: [T.Text]) id maybeSystems
                            jsonOut systems
                            

startService :: Config -> IO ()
startService c = do 
                   let port = (restPort c)
                   let path = (restPath c)
                   let tpath = TL.pack path
                   allSystemsCache <- newCache (restCacheTimeout c)
                   infoM "EDDA.Rest" ("Rest service started on port: " ++ (show port))
                   scotty port $ do
                                  middleware $ gzip def
                                  systemsWithinLy allSystemsCache c path
                                  get (servicePath path "") $ do
                                                        html $ mconcat ["<html><title>EDDA REST API</title><body><h2>EDDA REST API</h2><br/><br/>",
                                                                        "<a href=\"",tpath,"/systemsWithinLy/:systemName/:distance\">",tpath,"/systemsWithinLy/:systemName/:distance</a><br/><br/>",
                                                                        "<br/><br/>Powered by <a href=\"http://hackage.haskell.org/package/scotty\">Scotty</a></body></html>"]

