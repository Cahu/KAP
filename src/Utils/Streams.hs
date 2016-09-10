{-# LANGUAGE FlexibleInstances #-}

module Utils.Streams
( monitorStreamWait
, monitorStreamChange
, displayStreamValues
) where


import KRPCHS
import KRPCHS.SpaceCenter

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
