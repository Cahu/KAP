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
import Control.Concurrent
import Control.Concurrent.Async


{- For this rocket, we need two independant subprograms :
 - one will handle staging, the other will handle guidance.
 -
 - We can't have both send requests to the server using the same connection or
 - we will have race conditions. To keep things simple, we open two distinct
 - connections.
 -}


main :: IO ()
main = do
    stagingTask <- async ascentStagingMain
    controlTask <- async ascentControlMain
    void $ waitAnyCancel [ stagingTask, controlTask ]


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
            targetAlt = 200000

            -- directions in the vessel's reference frame
            down    = V3 0 0 1    :: V3 Double
            forward = V3 0 1 0    :: V3 Double
            right   = V3 1 0 0    :: V3 Double
            up      = negated down
            left    = negated right

            -- directions in the orbital reference frame
            prograde = V3 0    1 0 :: V3 Double
            normal   = V3 0    0 1 :: V3 Double
            radial   = V3 (-1) 0 0 :: V3 Double

            -- directions in the surface reference frame
            surfaceUp    = V3 1 0 0 :: V3 Double
            surfaceNorth = V3 0 1 0 :: V3 Double
            surfaceEast  = V3 0 0 1 :: V3 Double

            -- helper functions
            pitch    = setControlPitch    control
            roll     = setControlRoll     control
            yaw      = setControlYaw      control
            throttle = setControlThrottle control
                
            rotToward dir msg = do
                (moiP, moiR, moiY) <- getStreamResult moiStream      msg
                (torP, torR, torY) <- getStreamResult torqueStream   msg
                (avX, avY, avZ)    <- getStreamResult angularVStream msg
                (qX, qY, qZ, qW)   <- getStreamResult rotStream msg
                let -- inverse rotation
                    q = conjugate $ Quaternion qW (V3 qX qY qZ)
                let -- bring the desired direction in the vessels' reference frame
                    dir'@(V3 x' y' z') = rotate q dir
                    sepPitch  = V3 0  y' z'
                    sepYaw    = V3 x' y' 0
                    anglPitch = signum (dir' `dot` down) * angleBetween forward sepPitch :: Double
                    anglYaw   = signum (dir' `dot` left) * angleBetween forward sepYaw   :: Double
                let -- find angular velocities on pitch, roll and yaw
                    V3 avPitch avRoll avYaw = rotate q (V3 avX avY avZ)
                --liftIO $ putStrLn $ printf "picth ~ angle: %.02g - av: %.02g - moi: %.02g - tor: %.02g" anglPitch avPitch moiP torP
                --liftIO $ putStrLn $ printf "yaw   ~ angle: %.02g - av: %.02g - moi: %.02g - tor: %.02g" anglYaw avYaw moiY torY
                pitch $ controlToStopAtAngle anglPitch avPitch moiP torP
                yaw   $ controlToStopAtAngle anglYaw   avYaw   moiY torY

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
                v <- getStreamResult vSpeedStream msg
                return $ v**2 / r

            changePitch msg angl = do
                let dir = V3 (cos angl) 0 (sin angl)
                rotToward dir msg

            pitchAccelRatio msg ratio = changePitch msg angl
                where angl = acos $ max (-0.5) $ min 0.5 $ ratio -- thrust angle

            loop = do
                msg    <- getStreamMessage streamClient
                accel  <- getAccel            msg
                grav   <- getGravity          msg
                cenA   <- getCentrifugalAccel msg
                alt    <- getStreamResult altStream msg
                vSpeed <- getStreamResult vSpeedStream msg
                hSpeed <- getStreamResult hSpeedStream msg
                let
                    vertA  = cenA - grav             -- total vertical acceleration (w/o engines)
                    altDif = targetAlt - alt         -- difference between our current altitude and the desired one
                    delta  = vSpeed**2 + 2*vertA*altDif

                liftIO $ printf "altitude: %.02g\n" alt
                liftIO $ printf "vAccel  : %.02g\n" vertA
                liftIO $ printf "vSpeed  : %.02g ~ hSpeed: %.02g\n" vSpeed hSpeed

                if | vertA > 0.01 -> do
                        liftIO $ putStrLn $ printf "IN ORBIT!"
                        changePitch msg (pi/4)

                   | accel == 0 -> do
                        pitch 0
                        yaw   0
                        loop

                   | (vSpeed < 100 && alt < 10000) -> do
                        changePitch msg (pi/2)
                        loop

                   | (alt < 90000) -> do
                        changePitch msg (deg 6 + deg 3 * alt / 4000)
                        loop

                   | delta <= 0 && vSpeed > 0 -> do
                        let dAcc = negate (vertA + vSpeed**2/(2*altDif))
                        liftIO $ putStrLn $ printf "TOO LOW: %.02g ~ dAcc: %.02g" delta dAcc
                        pitchAccelRatio msg (dAcc / accel)
                        loop

                   | delta <= 0 -> do
                        liftIO $ putStrLn $ printf "FALLING BACK!"
                        pitchAccelRatio msg 1
                        loop

                   | otherwise -> do
                        let t1 = (negate vSpeed + sqrt delta) / vertA
                            t2 = (negate vSpeed - sqrt delta) / vertA
                            tm = max t1 t2
                        if tm > 0 then do
                                let dAcc = negate (vSpeed / (tm**2) + vertA/(1+(tm/30)**2))
                                liftIO $ putStrLn $ printf "OK: %.02g ~ t1: %.02g ~ t2: %.02g" delta t1 t2
                                pitchAccelRatio msg (dAcc / accel)
                                loop
                        else do
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

        waitIndefinitelly = forever $
            liftIO $ threadDelay 1000000

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

        waitIndefinitelly


reentryProg :: StreamClient -> RPCContext ()
reentryProg streamClient = getStandardVesselPackage >>= \StandardVesselPackage{..} ->
    let
        stage = void $ controlActivateNextStage control

        waitSafeAltitude altStream = keepTryingOnExcept NoSuchStream $
            monitorStreamWait streamClient altStream (< (7000))
    in do
        withStream (getFlightSurfaceAltitudeStreamReq flight) waitSafeAltitude
        stage -- activate chutes
