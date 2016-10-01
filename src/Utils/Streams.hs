{-# LANGUAGE FlexibleInstances #-}

module Utils.Streams
( monitorStreamWait
, monitorStreamChange
, displayStreamValues

, getStreamResultWith
, v2FromTuple
, v3FromTuple
, v4FromTuple
, quaternionFromTuple

) where


import KRPCHS
import KRPCHS.SpaceCenter

import Linear

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.Chan


class StreamMessageSource a where
    getMessage :: a -> RPCContext KRPCStreamMsg


instance StreamMessageSource StreamClient where
    getMessage = getStreamMessage

instance StreamMessageSource (Chan KRPCStreamMsg) where
    getMessage = liftIO . readChan


monitorStreamWait :: (KRPCResponseExtractable a, StreamMessageSource s) => s -> KRPCStream a -> (a -> Bool) -> RPCContext ()
monitorStreamWait source stream predicate = loop
    where
        loop = do
            msg <- getMessage source
            val <- getStreamResult stream msg
            unless (predicate val) loop


monitorStreamChange :: (KRPCResponseExtractable a, StreamMessageSource s) => s -> KRPCStream a -> (a -> a -> Bool) -> RPCContext ()
monitorStreamChange source stream predicate = loop Nothing
    where
        loop maybeVal = do
            msg <- getMessage source
            val <- getStreamResult stream msg
            case maybeVal of
                Nothing     -> loop (Just val)
                Just oldVal -> unless (predicate oldVal val) (loop $ Just val)


displayStreamValues :: (Show a, KRPCResponseExtractable a, StreamMessageSource s) => s -> KRPCStream a -> RPCContext ()
displayStreamValues source stream = loop
    where
        loop = do
            msg <- getMessage source
            val <- getStreamResult stream msg
            liftIO $ print val
            loop


getStreamResultWith :: KRPCResponseExtractable a => (a -> b) -> KRPCStream a -> KRPCStreamMsg -> RPCContext b
getStreamResultWith f stream msg = f <$> getStreamResult stream msg


v2FromTuple :: Num a => (a, a) -> V2 a
v2FromTuple (a, b) = V2 a b

v3FromTuple :: Num a => (a, a, a) -> V3 a
v3FromTuple (a, b, c) = V3 a b c

v4FromTuple :: Num a => (a, a, a) -> V3 a
v4FromTuple (a, b, c) = V3 a b c

quaternionFromTuple :: Num a => (a, a, a, a) -> Quaternion a
quaternionFromTuple (x, y, z, w) = Quaternion w (V3 x y z)
