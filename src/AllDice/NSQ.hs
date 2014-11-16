{-# LANGUAGE OverloadedStrings #-}
module AllDice.NSQ
    ( nsqApp
    ) where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import Data.Aeson
import System.Log.Logger (infoM, errorM)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

-- NSQ endpoint
import qualified Network.NSQ as NSQ

-- Not ideal but should in theory work for now
import System.Random

-- Scheme interpreter
import AllDice.Scheme


nsqApp :: NSQ.NSQConnection -> T.Text -> IO ()
nsqApp conf stdlib = do
    -- Queue
    -- Queue
    topicQueue <- newTQueueIO
    replyQueue <- newTQueueIO

    -- Inject the channel + rdy
    atomically $ do
        writeTQueue replyQueue $ NSQ.Sub "alldice" "lethalcode.net" True
        writeTQueue replyQueue $ NSQ.Rdy 8 -- TODO: fixed rdy of 8 threads

    -- Connect
    race_
        (NSQ.establish conf topicQueue replyQueue)
        (consumeMessages stdlib topicQueue replyQueue)


consumeMessages :: T.Text -> TQueue NSQ.Message -> TQueue NSQ.Command -> IO ()
consumeMessages stdlib q r = forever $ do
    msg <- atomically (do
        m <- readTQueue q

        writeTQueue r $ NSQ.Fin $ mId m
        writeTQueue r $ NSQ.Rdy 1
        return m)

    infoM "Client.Consume" (show msg)

    case eitherDecode' (BL.fromStrict (mMsg msg)) of
        -- Emit an error message
        Left err  -> errorM "Client.Consume" ("Cannot decode json message -- " ++ show msg)
        Right req -> do
            -- Run the scheme interpreter here in runST & runRand then return the result
            gen <- liftIO newStdGen

            -- Load the stdlib
            stdlib <- liftIO $ T.readFile (T.unpack stdlib)

            let roll = runST $ runExpr stdlib (expr req) gen

            -- Emit a message
            atomically $ writeTQueue r $ NSQ.Pub (replyQ req) (BL.toStrict $ encode $ Reply (expr req) roll "")

    where
        mId (NSQ.Message _ _ mesgId _) = mesgId
        mMsg (NSQ.Message _ _ _ m) = m


data Request = Request
    { expr :: T.Text
    , replyQ :: T.Text
    } deriving (Show)

instance FromJSON Request where
    parseJSON (Object v) =
        Request
        <$> (v .: "expression")
        <*> (v .: "replyQueue")
    parseJSON _ = mzero


data Reply = Reply
    { exprR :: T.Text
    , resultR :: T.Text
    , errorR :: T.Text
    } deriving (Show)

instance ToJSON Reply where
    toJSON p = object
        [ "expression" .= exprR p
        , "result" .= resultR p
        , "error" .= errorR p
        ]
