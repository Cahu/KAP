{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib 
import KRPCHS
import KRPCHS.SpaceCenter

import Linear

import Text.Printf
import Control.Monad
import Control.Monad.Trans

import System.Environment


{- For this rocket, we need two independant subprograms :
 - one will handle staging, the other will handle guidance.
 -
 - We can't have both send requests to the server using the same connection or
 - we will have race conditions. To keep things simple, we open two distinct
 - connections.
 -}


main :: IO ()
main =
    withRPCClient    "5_DeorbitBurn" "127.0.0.1" "50000" $ \client       ->
    withStreamClient client          "127.0.0.1" "50001" $ \streamClient -> do
        hT   <- getUllageTime
        node <- runRPCProg client getNextNode
        runRPCProg client (execNode streamClient node hT)


getUllageTime :: IO Double
getUllageTime = do
    args <- getArgs
    case args of
        (x:_) -> return (read x)
        _     -> fail "no ullage time provided"

getNextNode :: RPCContext Node
getNextNode = getStandardVesselPackage >>= \StandardVesselPackage{..} -> do
    nodes <- getControlNodes control
    case nodes of
        (n:_) -> return n
        _     -> fail "No nodes found"


execNode :: StreamClient -> Node -> Double -> RPCContext ()
execNode streamClient node ullageTime =
    getStandardVesselPackage              >>= \StandardVesselPackage{..} ->
    getVesselAvailableThrust       vessel >>= \thrust                    ->
    getVesselOrbitalReferenceFrame vessel >>= \orbitalRef                ->
    vesselFlight vessel orbitalRef        >>= \orbitalFlight             ->
      withStream (nodeRemainingBurnVectorStreamReq  node   orbitalRef) $ \burnVectStream  ->
      withStream (getNodeTimeToStreamReq            node             ) $ \burnTimeStream  ->
      withStream (vesselAngularVelocityStreamReq    vessel orbitalRef) $ \avStream        ->
      withStream (getVesselAvailableTorqueStreamReq vessel           ) $ \torqueStream    ->
      withStream (getVesselMomentOfInertiaStreamReq vessel           ) $ \moiStream       ->
      withStream (getVesselMassStreamReq            vessel           ) $ \massStream      ->
      withStream (getFlightRotationStreamReq        orbitalFlight    ) $ \rotStream       ->
        let
            attitudeCtrl = AttitudeControl
                { attControl      = control
                , attAvStream     = avStream
                , attRotStream    = rotStream
                , attMoiStream    = moiStream
                , attTorqueStream = torqueStream
                }

            -- helper functions
            pitch    = setControlPitch    control
            forward  = setControlForward  control
            roll     = setControlRoll     control
            yaw      = setControlYaw      control
            throttle = setControlThrottle control
            rcs      = setControlRCS      control

            stop = do
                rcs False
                throttle 0
                forward  0
                pitch 0
                yaw   0

            burn ratio = do
                rcs False
                forward 0
                throttle ratio

            ullage = do
                rcs True
                throttle 0
                forward 1

            getAccel msg = do
                mass <- realToFrac <$> getStreamResult massStream msg
                return $ thrust / mass

            getBurnDir msg = do -- burn vector in the vessel's reference frame
                burnVect <- v3FromTuple         <$> getStreamResult burnVectStream msg
                q        <- quaternionFromTuple <$> getStreamResult rotStream      msg
                return $ rotate (conjugate q) burnVect

            loop ullageTime = keepTryingOnExcept NoSuchStream $ do
                msg <- getStreamMessage streamClient
                loop' =<< getBurnDir msg
              where
                minThrottle = 0.4
                loop' initVect = do
                    msg      <- getStreamMessage streamClient
                    accel    <- getAccel   msg
                    burnDir  <- getBurnDir msg
                    timeTo   <- getStreamResult burnTimeStream msg
                    burnVect <- v3FromTuple <$> getStreamResult burnVectStream msg
                    let deltaV       = realToFrac $ norm burnVect
                        tBurn        = realToFrac $ deltaV / accel
                        dAcc         = if tBurn > 0.5 then accel else deltaV / 0.5
                        ratio        = realToFrac $ max minThrottle (dAcc / accel)
                        maxPrecision = realToFrac $ minThrottle*accel*0.02
                    liftIO $ printf "Time: %.02g s ~ dV %.02g m/s +/- %.02g m/s (%.02g s)\n" timeTo deltaV maxPrecision tBurn
                    _   <- setRotationRpm 5 attitudeCtrl msg
                    ang <- rotToward burnVect attitudeCtrl msg
                    if | (abs (angleBetween burnDir initVect) > 0.5)  -> stop
                       | (tBurn  < maxPrecision)                      -> ullage     >> loop' initVect -- max precision of the burn
                       | (ang    > 0.2)                               -> loop' burnDir
                       | (timeTo < tBurn / 2)                         -> burn ratio >> loop' initVect
                       | (timeTo < ullageTime + tBurn / 2)            -> ullage     >> loop' initVect
                       | otherwise                                    -> loop' burnDir

        in
            if (thrust == 0) then
                 fail "No acceleration possible"
            else
                loop ullageTime

