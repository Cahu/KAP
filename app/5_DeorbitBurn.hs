{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Lib 
import KRPCHS
import KRPCHS.SpaceCenter

import Linear

import System.Environment

import Text.Printf
import Control.Monad
import Control.Monad.Trans


{- For this rocket, we need two independant subprograms :
 - one will handle staging, the other will handle guidance.
 -
 - We can't have both send requests to the server using the same connection or
 - we will have race conditions. To keep things simple, we open two distinct
 - connections.
 -}


main :: IO ()
main =
    getBurnTime >>= \burnTime ->
    withRPCClient    "5_DeorbitBurn" "127.0.0.1" "50000" $ \client ->
    withStreamClient client          "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (deorbitProg streamClient burnTime)


getBurnTime :: IO Double
getBurnTime = getArgs >>= \args ->
    if null args then error "No burn time supplied"
                 else return $ read (head args)


deorbitProg :: StreamClient -> Double -> RPCContext ()
deorbitProg streamClient burnTime =
    getStandardVesselPackage              >>= \StandardVesselPackage{..} ->
    getVesselOrbitalReferenceFrame vessel >>= \orbitalRef                ->
    vesselFlight vessel orbitalRef        >>= \orbitalFlight             ->
      withStream (getUTStreamReq                                     ) $ \utStream        ->
      withStream (vesselAngularVelocityStreamReq    vessel orbitalRef) $ \avStream        ->
      withStream (getVesselAvailableTorqueStreamReq vessel           ) $ \torqueStream    ->
      withStream (getVesselMomentOfInertiaStreamReq vessel           ) $ \moiStream       ->
      withStream (getFlightRotationStreamReq        orbitalFlight    ) $ \rotStream       ->
      withStream (getFlightMeanAltitudeStreamReq    flight)            $ \altStream       ->
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
            roll     = setControlRoll     control
            yaw      = setControlYaw      control
            throttle = setControlThrottle control

            pointAt at = keepTryingOnExcept NoSuchStream $
                pointAtLoop
              where
                pointAtLoop = do
                    msg <- getStreamMessage streamClient
                    q   <- quaternionFromTuple <$> getStreamResult rotStream msg
                    av  <- v3FromTuple         <$> getStreamResult avStream  msg
                    let V3 avP _ avY = rotate q av
                        dir          = rotate q vesselForward
                        ang          = angleBetween dir at
                        angVel       = norm (V2 avP avY)
                    if (abs ang > 0.05 || angVel > 0.1) then do
                        rotToward at attitudeCtrl msg
                        pointAtLoop
                    else do
                        pitch 0
                        yaw   0

            maintainDirFor maintainDir nSec = do
                pointAt maintainDir
                maintainLoop 0
              where
                maintainLoop startUT = keepTryingOnExcept NoSuchStream $ do
                    msg <- getStreamMessage streamClient
                    ut  <- getStreamResult utStream msg
                    rotToward maintainDir attitudeCtrl msg 
                    if | (startUT == 0 || nSec < 0) -> maintainLoop ut
                       | (ut - startUT < nSec)      -> maintainLoop startUT
                       | otherwise                  -> pitch 0 >> yaw 0

            waitBelowAltitude alt = keepTryingOnExcept NoSuchStream $
                monitorStreamWait streamClient altStream (< alt)

        in do
            liftIO $ putStrLn "Stabilizing fuel ..."
            maintainDirFor radial     0.2
            maintainDirFor retrograde 0.2

            liftIO $ putStrLn "Burn ..."
            setControlThrottle control 0.5
            maintainDirFor retrograde burnTime
            setControlThrottle control 0

            liftIO $ putStrLn "Waiting to reach 107km ..."
            waitBelowAltitude 107000

            liftIO $ putStrLn "Staging ..."
            maintainDirFor radial 0.2
            void $ controlActivateNextStage control

            liftIO $ putStrLn "Butt first ..."
            maintainDirFor retrograde 240
