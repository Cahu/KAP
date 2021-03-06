{-# LANGUAGE RecordWildCards #-}

module Lib
( StandardVesselPackage(..)
, getStandardVesselPackage
, keepTryingOnExcept

, countDown
, countDownFromTo

, module Utils.Streams
, module Utils.Vectors
, module Control.Attitude

) where


import KRPCHS
import KRPCHS.UI
import KRPCHS.SpaceCenter

import Utils.Streams
import Utils.Vectors
import Control.Attitude

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Concurrent

import qualified Data.Text as T


data StandardVesselPackage = StandardVesselPackage
    { vessel  :: Vessel
    , control :: Control
    , flight  :: Flight
    , orbit   :: Orbit
    , body    :: CelestialBody
    }


getStandardVesselPackage :: RPCContext StandardVesselPackage
getStandardVesselPackage = do
    vessel  <- getActiveVessel
    control <- getVesselControl vessel
    orbit   <- getVesselOrbit vessel
    body    <- getOrbitBody orbit
    bodyRef <- getCelestialBodyReferenceFrame body
    flight  <- vesselFlight vessel bodyRef
    return $ StandardVesselPackage{..}


keepTryingOnExcept :: ProtocolError -> RPCContext a -> RPCContext a
keepTryingOnExcept exception action = loop
    where
        loop = do
            ok <- try action
            case ok of
                Left err -> if (err == exception) then loop else throwM err
                Right a  -> return a


countDown :: Int -> RPCContext ()
countDown n = countDownFromTo n 1


countDownFromTo :: Int -> Int -> RPCContext ()
countDownFromTo m n = forM_ (reverse [ n .. m ]) $ \sec -> do
    void   $ message (T.pack $ show sec) 1 MessagePosition'TopCenter
    liftIO $ threadDelay 1000000
