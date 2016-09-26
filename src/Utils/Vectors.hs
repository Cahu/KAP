module Utils.Vectors
( deg
, angleBetween
) where


import Linear

deg :: Double -> Double
deg angl = angl*pi/180

angleBetween :: (Metric f, Floating a) => f a -> f a -> a
angleBetween v1 v2 = acos $ (v1 `dot` v2) / (norm v1 * norm v2)
