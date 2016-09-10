{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib

import KRPCHS
import KRPCHS.SpaceCenter

import Control.Monad
import Control.Monad.Trans


{- Slight modification of the previous design to allow transporting 4 kerbals
 - for suborbital tourism contracts. This uses two small radial SRBs (in
 - addition to the main SR motor) which are jettisoned when a sudden drop in
 - thrust is detected.
 -
 - Again, no control on this one. The rocket uses fins and TWR tweaking.
 -}

main :: IO ()
main =
    withRPCClient    "3_RadialSRBs" "127.0.0.1" "50000" $ \client ->
    withStreamClient client         "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (mainProg streamClient)


mainProg :: StreamClient -> RPCContext ()
mainProg streamClient =
    getStandardVesselPackage >>= \StandardVesselPackage{..} ->

    let stage = void $ controlActivateNextStage control

        -- wait for a sudden drop of thrust
        waitBoosterBurnout thrustStream = keepTryingOnExcept NoSuchStream $
            monitorStreamChange streamClient thrustStream (\oldval newval -> oldval - newval > 100)

        waitStartFalling vSpeedStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient vSpeedStream (< (-10))

        waitSafeAltitude altStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient altStream (< (7000))

    in do
        stage -- launch
        withStream (getVesselAvailableThrustStreamReq vessel) waitBoosterBurnout
        stage -- decouble boosters
        withStream (getFlightVerticalSpeedStreamReq   flight) waitStartFalling
        stage -- decouple pod
        withStream (getFlightSurfaceAltitudeStreamReq flight) waitSafeAltitude
        stage -- activate chutes


