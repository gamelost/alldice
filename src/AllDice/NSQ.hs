{-# LANGUAGE OverloadedStrings #-}
module AllDice.NSQ
    ( consumeMessages
    ) where

import Control.Concurrent.STM
import Control.Monad
import System.Log.Logger (infoM)
import qualified Data.Text as T

-- NSQ endpoint
import qualified Network.NSQ as NSQ

-- Not ideal but should in theory work for now
import System.Random

-- Scheme interpreter
import AllDice.Scheme


consumeMessages :: T.Text -> TQueue NSQ.Message -> TQueue NSQ.Command -> IO ()
consumeMessages path q r = forever $ do
    msg <- atomically (do
        m <- readTQueue q
        -- TODO: Process data here

        -- TODO: Unsafe, assumes it only get Messages (true as of current implementation, but still unsafe)
        writeTQueue r $ NSQ.Fin $ mId m
        return m)
    infoM "Client.Consume" (show msg)

    where
        mId (NSQ.Message _ _ mesgId _) = mesgId
