{-# LANGUAGE RecordWildCards #-}

module Lib
( StandardVesselPackage(..)
, getStandardVesselPackage
, keepTryingOnExcept
, monitorStreamWait
) where


import KRPCHS
import KRPCHS.SpaceCenter

import Control.Monad
import Control.Monad.Catch


data StandardVesselPackage = StandardVesselPackage
    { vessel  :: Vessel
    , control :: Control
    , flight  :: Flight
    , orbit   :: Orbit
    , body    :: CelestialBody
    , bodyRef :: ReferenceFrame
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
            

monitorStreamWait :: KRPCResponseExtractable a => StreamClient -> KRPCStream a -> (a -> Bool) -> RPCContext ()
monitorStreamWait client stream predicate = loop
    where
        loop = do
            msg <- getStreamMessage client
            val <- getStreamResult stream msg
            unless (predicate val) loop
