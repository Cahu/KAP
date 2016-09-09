{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib

import KRPCHS
import KRPCHS.SpaceCenter

import Control.Monad
import Control.Monad.Trans


main :: IO ()
main =
    withRPCClient    "1_PodOnFlea" "127.0.0.1" "50000" $ \client ->
    withStreamClient client        "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (mainProg streamClient)


mainProg :: StreamClient -> RPCContext ()
mainProg streamClient =
    getStandardVesselPackage >>= \StandardVesselPackage{..} ->

    let stage = void $
            controlActivateNextStage control

        waitStartFalling vSpeedStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient vSpeedStream (< (-10))

    in
        withStream (getFlightVerticalSpeedStreamReq flight) $ \verticalSpeedStream -> do
            stage                                -- launch
            waitStartFalling verticalSpeedStream -- wait til we start falling back
            stage                                -- activate chutes
