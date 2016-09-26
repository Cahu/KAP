{-# LANGUAGE MultiWayIf #-}

module Utils.Physics
( Delay
, Torque
, AngularVelocity
, MomentOfInertia

, delayToStopAv
, controlToStopAv
, controlToStopAtAngle
) where


type Delay           = Double
type AngularVelocity = Double
type MomentOfInertia = Double
type Torque          = Double


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

