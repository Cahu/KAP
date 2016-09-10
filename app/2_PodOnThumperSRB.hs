{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib

import KRPCHS
import KRPCHS.SpaceCenter

import Control.Monad
import Control.Monad.Trans

{- This program is designed to work with a simple rocket built for suborbital flight.
 -
 - The program expects 3 staging events for the rocket :
 - * launch stage with a SRB
 - * decoupling
 - * chutes deployment
 -
 - There is no guidance: use fins and tweak the TWR.
 -}

main :: IO ()
main =
    withRPCClient    "2_PodOnThumper" "127.0.0.1" "50000" $ \client ->
    withStreamClient client           "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (mainProg streamClient)


mainProg :: StreamClient -> RPCContext ()
mainProg streamClient =
    getStandardVesselPackage >>= \StandardVesselPackage{..} ->

    let stage = void $ controlActivateNextStage control

        waitStartFalling vSpeedStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient vSpeedStream (< (-10))

        waitSafeAltitude altStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient altStream (< (7000))

    in
        withStream (getFlightVerticalSpeedStreamReq   flight) $ \verticalSpeedStream ->
        withStream (getFlightSurfaceAltitudeStreamReq flight) $ \altitudeStream      -> do
            stage                                -- launch
            waitStartFalling verticalSpeedStream -- wait til we start falling back
            stage                                -- decouple
            waitSafeAltitude altitudeStream      -- wait for safe altitude to deploy chutes
            stage                                -- deploy chutes

