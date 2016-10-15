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
import Control.Concurrent.Async


{- For this rocket, we need two independant subprograms :
 - one will handle staging, the other will handle guidance.
 -
 - We can't have both send requests to the server using the same connection or
 - we will have race conditions. To keep things simple, we open two distinct
 - connections.
 -}


main :: IO ()
main =
    withAsync ascentControlMain $ \controlTask ->
    withAsync ascentStagingMain $ \stagingTask -> do
        wait stagingTask
        wait controlTask

ascentControlMain :: IO ()
ascentControlMain =
    withRPCClient    "4_ControlledAscent_Control" "127.0.0.1" "50000" $ \client ->
    withStreamClient client                       "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (controlProg streamClient)

ascentStagingMain :: IO ()
ascentStagingMain =
    withRPCClient    "4_ControlledAscent_Staging" "127.0.0.1" "50000" $ \client ->
    withStreamClient client                       "127.0.0.1" "50001" $ \streamClient ->
        runRPCProg client (stagingProg streamClient)


controlProg :: StreamClient -> RPCContext ()
controlProg streamClient =
    getStandardVesselPackage >>= \StandardVesselPackage{..} ->
    getVesselSurfaceReferenceFrame vessel          >>= \surfaceRef    ->
    getCelestialBodyEquatorialRadius body          >>= \bodyRadius    ->
    getCelestialBodyMass             body          >>= \bodyMass      ->
    getCelestialBodyNonRotatingReferenceFrame body >>= \bodyRef       ->
    vesselFlight vessel surfaceRef                 >>= \surfaceFlight ->
    vesselFlight vessel bodyRef                    >>= \bodyFlight    ->
      withStream (getFlightMeanAltitudeStreamReq    surfaceFlight)     $ \altStream       ->
      withStream (getFlightHorizontalSpeedStreamReq bodyFlight)        $ \hSpeedStream    ->
      withStream (getFlightVerticalSpeedStreamReq   bodyFlight)        $ \vSpeedStream    ->
      withStream (getVesselThrustStreamReq          vessel)            $ \thrustStream    ->
      withStream (getVesselMassStreamReq            vessel)            $ \massStream      ->
      withStream (vesselAngularVelocityStreamReq    vessel surfaceRef) $ \angularVStream  ->
      withStream (getVesselAvailableTorqueStreamReq vessel)            $ \torqueStream    ->
      withStream (getVesselMomentOfInertiaStreamReq vessel)            $ \moiStream       ->
      withStream (getFlightRotationStreamReq surfaceFlight)            $ \rotStream       ->
        let
            bigG      = 6.674 * 10**(-11)
            incl      = deg 6.2 -- desired orbit inclination
            targetAlt = 150000

            attitudeCtrl = AttitudeControl
                { attControl      = control
                , attAvStream     = angularVStream
                , attRotStream    = rotStream
                , attMoiStream    = moiStream
                , attTorqueStream = torqueStream
                }

            -- helper functions
            pitch    = setControlPitch    control
            roll     = setControlRoll     control
            yaw      = setControlYaw      control
            throttle = setControlThrottle control

            getAccel msg = do
                mass   <- realToFrac <$> getStreamResult massStream msg
                thrust <- realToFrac <$> getStreamResult thrustStream msg
                return $ thrust / mass

            getR msg = (+ realToFrac bodyRadius)
                <$> getStreamResult altStream msg

            getGravity msg = do
                r <- getR msg
                return $ realToFrac (bigG * bodyMass) / r**2

            getCentrifugalAccel msg = do
                r <- getR msg
                v <- getStreamResult hSpeedStream msg
                return $ v**2 / r

            changePitch msg angl = do
                let dir = V3 (cos angl) (sin angl * sin incl) (sin angl * cos incl)
                rotToward dir attitudeCtrl msg

            pitchAccelRatio msg ratio = changePitch msg angl
                where angl = acos $ max (-0.5) $ min 0.5 $ ratio -- thrust angle

            loop = do
                msg <- getStreamMessage streamClient

                accel  <- getAccel            msg
                grav   <- getGravity          msg
                cenA   <- getCentrifugalAccel msg
                alt    <- getStreamResult altStream msg
                vSpeed <- getStreamResult vSpeedStream msg
                hSpeed <- getStreamResult hSpeedStream msg

                let vertA  = cenA - grav      -- total vertical acceleration (w/o engines)
                    altDif = targetAlt - alt  -- difference between our current altitude and the desired one
                    delta  = vSpeed**2 + 2*vertA*altDif
                    t1     = if delta < 0 then 0 else (negate vSpeed + sqrt delta) / vertA
                    t2     = if delta < 0 then 0 else (negate vSpeed - sqrt delta) / vertA
                    tm     = max t1 t2

                liftIO $ printf "altitude: %.02g\n" alt
                liftIO $ printf "vAccel  : %.02g\n" vertA
                liftIO $ printf "vSpeed  : %.02g ~ hSpeed: %.02g\n" vSpeed hSpeed

                if | (vertA > 0.01) ->  do -- when in orbit
                        liftIO $ putStrLn $ printf "IN ORBIT!"
                        pitch 0
                        yaw   0

                   | (accel == 0) -> do -- what to do when not accelerating
                        pitch 0
                        yaw   0
                        loop

                   | (vSpeed < 100 && alt < 10000) -> do -- what to do on the first stage of the launch (before gravity turn)
                        changePitch msg 0
                        loop

                   | (alt < 90000) -> do -- flight while in the atmosphere, 'assisted gravity turn'
                        changePitch msg (deg 6 + deg 3 * alt / 4000)
                        loop

                   | (delta <= 0 && vSpeed > 0) -> do -- when adjusting apoapsis
                        let dAcc = 1 - (vertA + vSpeed**2/(2*altDif))
                        liftIO $ putStrLn $ printf "TOO LOW: %.02g ~ dAcc: %.02g" delta dAcc
                        pitchAccelRatio msg (dAcc / accel)
                        loop

                   | (delta > 0 && tm > 0) -> do -- apoapsis ok and there is time before falling back to desired altitude
                        let dAcc   = negate (vSpeed/tm + vertA / (1+(tm/60)**2))
                        liftIO $ putStrLn $ printf "OK: %.02g ~ t1: %.02g ~ t2: %.02g" delta t1 t2
                        pitchAccelRatio msg (dAcc / accel)
                        loop

                    | otherwise -> do -- if here, then we are falling back toward the planet/moon
                        liftIO $ putStrLn $ printf "FALLING BACK!"
                        pitchAccelRatio msg 1
                        loop

        in
            keepTryingOnExcept NoSuchStream $ do
                throttle 1
                loop
                throttle 0


stagingProg :: StreamClient -> RPCContext ()
stagingProg streamClient =
    getStandardVesselPackage >>= \StandardVesselPackage{..} ->

    let stage = void $ controlActivateNextStage control

        -- wait for a sudden drop of thrust
        waitThrustDrop thrustStream = keepTryingOnExcept NoSuchStream $
            monitorStreamChange streamClient thrustStream (\oldval newval -> oldval - newval > 100)

        waitStartFalling vSpeedStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient vSpeedStream (< (-10))

    in do
        countDownFromTo 5 3
        stage -- fire

        countDownFromTo 2 1
        stage -- release clamps

        withStream (getVesselAvailableThrustStreamReq vessel) $ \thrustStream -> do
            waitThrustDrop thrustStream
            stage -- decouple boosters
            waitThrustDrop thrustStream
            stage -- 1st stage separation
            waitThrustDrop thrustStream
            stage -- 2nd stage separation
