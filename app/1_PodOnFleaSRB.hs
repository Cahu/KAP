module Main where

import KRPCHS
import KRPCHS.SpaceCenter

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans


main :: IO ()
main =
    withRPCClient    "1_PodOnFlea" "127.0.0.1" "50000" $ \client ->
    withStreamClient client        "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (mainProg streamClient)


mainProg :: StreamClient -> RPCContext ()
mainProg streamClient =
    getActiveVessel                       >>= \vessel  ->
    getVesselControl vessel               >>= \control ->
    getVesselOrbit vessel                 >>= \orbit   ->
    getOrbitBody orbit                    >>= \planet  ->
    getCelestialBodyReferenceFrame planet >>= \ref     ->
    vesselFlight vessel ref               >>= \flight  ->

    let
        stage = void $ controlActivateNextStage control

        monitorWait stream predicate = loop
            where loop = do
                    msg <- getStreamMessage streamClient
                    ok  <- try (getStreamResult stream msg)
                    case ok of
                        Left NoSuchStream -> loop
                        Left e            -> throwM e
                        Right val         -> unless (predicate val) loop

    in
        withStream (getFlightVerticalSpeedStreamReq flight) $ \verticalSpeedStream -> do
            stage                                     -- launch
            monitorWait verticalSpeedStream (< (-10)) -- monitor vertical speed : wait until we start falling
            stage                                     -- activate chutes
