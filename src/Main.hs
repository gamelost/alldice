{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Main where

import Control.Applicative
import Control.Concurrent (killThread)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
import qualified Control.Exception as E

import Data.Aeson hiding (json, String, Number)
import Data.IORef
import Data.STRef
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.ContentTypes

import System.Environment
import System.IO

-- Ekg monitoring
import System.Remote.Monitoring

-- NSQ endpoint
import qualified Network.NSQ as NSQ

-- Logger
import System.IO (stderr, Handle)
import System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger, Priority(DEBUG), setLevel, infoM)
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

-- Not ideal but should in theory work for now
import System.Random


import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Primitives


-- The Site argument
type SchemeRandom = StdGen
data MyRoute = MyRoute (IORef SchemeRandom) (T.Text)

-- Make MyRoute Routable
mkRoute "MyRoute" [parseRoutes|
/             HomeR           GET
/roll         RollR           GET
|]


-- Handlers

-- Util: Fetch the rng gen
getRng :: HandlerM MyRoute SchemeRandom
getRng = do
  MyRoute dbref _ <- master
  liftIO $ readIORef dbref

setRng :: SchemeRandom -> HandlerM MyRoute ()
setRng rng = do
  MyRoute dbref _ <- master
  liftIO $ writeIORef dbref rng

getFile :: HandlerM MyRoute T.Text
getFile = do
  MyRoute _ path <- master
  liftIO $ T.readFile (T.unpack path)

-- Display the possible actions
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ do
  json $ M.fromList (
                 [ ("description", "URI endpoint")
                 , ("home", showRoute HomeR)
                 , ("roll", showRoute RollR)
                 ] :: [(Text, Text)])

-- Perform a simple roll
getRollR :: Handler MyRoute
getRollR = runHandlerM $ do
    -- Get the source to run
    req <- request
    let query = queryString req :: [(B.ByteString, Maybe B.ByteString)]
        src = liftM T.decodeUtf8 $ join $ lookup "src" query :: Maybe Text -- TODO: will raise exception

    case src of
        Nothing -> json $ M.fromList (
                      [ ("description", "Scheme Dice Roll")
                      , ("input", "")
                      , ("error", "No source provided")
                      , ("output", "")
                      ] :: [(Text, Text)])

        Just val -> do
            -- Run the scheme interpreter here in runST & runRand then return the result
            gen <- liftIO newStdGen

            -- Load the stdlib
            stdlib <- getFile
            let roll = runST $ runExpr stdlib val gen

            -- TODO: debug
--            liftIO $ putStrLn $ T.unpack roll

            json $ M.fromList (
                  [ ("description", "Scheme Dice Roll")
                  , ("input", val)
                  , ("error", "")
                  , ("output", roll)
                  ] :: [(Text, Text)])


evalString :: LispEnv s -> T.Text -> ST s T.Text
evalString env expr = do
    exp <- evalExpr env expr
    case exp of
        Left err  -> return $ (T.pack . show) err
        Right val -> return $ (T.pack . show) val

evalExpr :: LispEnv s -> T.Text -> ST s (ThrowsError (LispVal s))
evalExpr env expr =
    case readExpr expr of
        Left err  -> return $ Left err
        Right val -> eval env val

evalFile :: LispEnv s -> T.Text -> ST s T.Text
evalFile env expr = do
    exps <- evalExprList env expr
    T.unlines `fmap` mapM (\exp -> case exp of
        Left err  -> return $ (T.pack . show) err
        Right val -> return $ (T.pack . show) val) exps

evalExprList :: LispEnv s -> T.Text -> ST s [ThrowsError (LispVal s)]
evalExprList env expr =
    case readExprList expr of
        Left err  -> return [Left err]
        Right val -> mapM (eval env) val

runExpr :: T.Text -> T.Text -> StdGen -> ST s T.Text
runExpr stdlib val gen = do
    env <- primitiveBindings

    -- Inject a val
    env' <- bindVars env [("stdRngGen", Random gen)]

    -- TODO: a nicer way to inject the stdlib into the env
    -- TODO: add error reporting for invalid/bad stdlib
    evalFile env' stdlib

    -- Run the scheme program given
    evalString env' val

-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: T.Text -> RouteM ()
application path = do
    gen <- liftIO $ getStdGen
    db <- liftIO $ newIORef gen
    middleware logStdoutDev
    route (MyRoute db path)
    defaultAction $ staticApp $ defaultFileServerSettings "static"


-- Perform a single roll
consumeMessages :: T.Text -> TQueue NSQ.Message -> TQueue NSQ.Command -> IO ()
consumeMessages path q r = forever $ do
    msg <- atomically (do
        m <- readTQueue q
        -- Process data here

        -- TODO: Unsafe, assumes it only get Messages (true as of current implementation, but still unsafe)
        writeTQueue r $ NSQ.Fin $ mId m
        return m)
    infoM "Client.Consume" (show msg)

    where
        mId (NSQ.Message _ _ mesgId _) = mesgId
--            -- Run the scheme interpreter here in runST & runRand then return the result
--            gen <- liftIO newStdGen
--
--            -- Load the stdlib
--            stdlib <- getFile
--  MyRoute _ path <- master
--  liftIO $ T.readFile (T.unpack path)
--
--            let roll = runST $ runExpr stdlib val gen
--
--            -- TODO: debug
----            liftIO $ putStrLn $ T.unpack roll
--
--            json $ M.fromList (
--                  [ ("description", "Scheme Dice Roll")
--                  , ("input", val)
--                  , ("error", "")
--                  , ("output", roll)
--                  ] :: [(Text, Text)])


-- Log Formatter
withFormatter :: GenericHandler System.IO.Handle -> GenericHandler System.IO.Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

-- Run the application
main :: IO ()
main = do
    -- Logger stuff
    stream <- withFormatter <$> streamHandler stderr DEBUG
    let loggerName = rootLoggerName

    updateGlobalLogger loggerName (setLevel DEBUG)
    updateGlobalLogger loggerName (setHandlers [stream])

    -- Ekg stuff
    putStrLn "Starting ekg monitoring on port 8081"
    ekgServer <- forkServer "localhost" 8081

    -- TODO: this bit is kinda gross but it lets us kill the ekg server when the wai or any other children threads dies.
    E.catch (race_
            (do
                putStrLn "Starting server on port 8080"
                toWaiApp (application "src/stdlib.scm") >>= run 8080
            )
            (do
                putStrLn "Starting nsq service"
                conf <- NSQ.defaultConfig "66.175.216.197"

                topicQueue <- newTQueueIO
                replyQueue <- newTQueueIO

                -- Connect
                race_
                    (NSQ.establish conf topicQueue replyQueue)
                    (consumeMessages "src/stdlib.scm" topicQueue replyQueue)
            )
        ) (\(E.ErrorCall e) -> (do
            putStrLn e
            killThread $ serverThreadId $ ekgServer
        ))

-- TODO:
--  - Limit size of incoming program 8KB maybe enough
--  - Cap cpu and memory consumption
--  - Tweak alloc to reduce, 8KB data yields 3GB/s allocs
--  - Consider migration to attoparsec for performance (Need benchmarks first)
