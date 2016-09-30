{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Attitude
( AttitudeControl(..)

, Direction
, Delay
, Torque
, AngularVelocity
, MomentOfInertia

, rotToward

, delayToStopAv
, controlToStopAv
, controlToStopAtAngle

-- directions in the orbital reference frame
, prograde
, normal
, radial

-- directions in the vessel's reference frame
, vesselDown
, vesselForward
, vesselRight
, vesselUp
, vesselLeft

-- directions in the surface reference frame
, surfaceUp
, surfaceNorth
, surfaceEast
) where


import Linear

import KRPCHS
import KRPCHS.SpaceCenter

import Utils.Vectors


type Delay           = Double
type AngularVelocity = Double
type MomentOfInertia = Double
type Torque          = Double
type Direction       = V3 Double


data AttitudeControl = AttitudeControl
    { attControl      :: Control
    , attAvStream     :: KRPCStream (Double, Double, Double)
    , attMoiStream    :: KRPCStream (Double, Double, Double)
    , attTorqueStream :: KRPCStream (Double, Double, Double)
    , attRotStream    :: KRPCStream (Double, Double, Double, Double)
    }


vesselDown :: Direction
vesselDown = V3 0 0 1

vesselForward :: Direction
vesselForward = V3 0 1 0

vesselRight :: Direction
vesselRight   = V3 1 0 0

vesselUp :: Direction
vesselUp = negated vesselDown

vesselLeft :: Direction
vesselLeft = negated vesselRight

prograde :: Direction
prograde = V3 0 1 0

normal :: Direction
normal = V3 0 0 1

radial :: Direction
radial = V3 (-1) 0 0

surfaceUp :: Direction
surfaceUp = V3 1 0 0

surfaceNorth :: Direction
surfaceNorth = V3 0 1 0

surfaceEast :: Direction
surfaceEast  = V3 0 0 1


delayToStopAv :: AngularVelocity -> MomentOfInertia -> Torque -> Delay
delayToStopAv av moi torque = abs av * moi / torque


controlToStopAv :: Delay -> AngularVelocity -> MomentOfInertia -> Torque -> Double
controlToStopAv delay av moi torque =
    let maxAccel = torque / moi
        accel    = if delayToStopAv av moi torque >= delay then maxAccel else av / delay
    in  signum av * abs accel / maxAccel


controlToStopAtAngle :: Fractional a => Double -> AngularVelocity -> MomentOfInertia -> Torque -> a
controlToStopAtAngle angl av moi tor = realToFrac $
    let stopDelay   = delayToStopAv av moi tor
        timeToPlane = abs $ if av /= 0 then angl / av else 100
    in
        if | abs angl < 0.001 && abs av < 0.01 -> 0
           | timeToPlane > stopDelay           -> controlToStopAv 0.5 (negate angl) moi tor
           | otherwise                         -> controlToStopAv stopDelay av moi tor


rotToward :: Direction -> AttitudeControl -> KRPCStreamMsg -> RPCContext ()
rotToward dir AttitudeControl{..} msg = do
    (moiP, moiR, moiY) <- getStreamResult attMoiStream    msg
    (torP, torR, torY) <- getStreamResult attTorqueStream msg
    ( avX,  avY,  avZ) <- getStreamResult attAvStream     msg
    (qX, qY, qZ, qW)   <- getStreamResult attRotStream    msg
    let -- inverse rotation
        q = conjugate $ Quaternion qW (V3 qX qY qZ)
    let -- bring the desired direction in the vessels' reference frame
        dir'@(V3 x' y' z') = rotate q dir
        sepPitch  = V3 0  y' z'
        sepYaw    = V3 x' y' 0
        anglPitch = signum (dir' `dot` vesselDown) * angleBetween vesselForward sepPitch :: Double
        anglYaw   = signum (dir' `dot` vesselLeft) * angleBetween vesselForward sepYaw   :: Double
    let -- find angular velocities on pitch, roll and yaw
        V3 avPitch avRoll avYaw = rotate q (V3 avX avY avZ)
    --liftIO $ putStrLn $ printf "picth ~ angle: %.02g - av: %.02g - moi: %.02g - tor: %.02g" anglPitch avPitch moiP torP
    --liftIO $ putStrLn $ printf "yaw   ~ angle: %.02g - av: %.02g - moi: %.02g - tor: %.02g" anglYaw avYaw moiY torY
    setControlPitch attControl $ controlToStopAtAngle anglPitch avPitch moiP torP
    setControlYaw   attControl $ controlToStopAtAngle anglYaw   avYaw   moiY torY
