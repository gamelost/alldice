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

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Web.Scotty
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger

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

-- Scheme interpreter
import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator


--
-- Scheme Handlers
--
evalString :: LispEnv s -> T.Text -> ST s T.Text
evalString env expr = do
    eexpr <- evalExpr env expr
    case eexpr of
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
    T.unlines `fmap` mapM (\eexp -> case eexp of
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
    _ <- evalFile env' stdlib

    -- Run the scheme program given
    evalString env' val


--
-- Scotty Handlers
--
scottyApplication :: T.Text -> ScottyM ()
scottyApplication path = do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    -- Home
    get "/" $ do
        file "src/root.html"
--        json $ M.fromList (
--            [ ("description", "URI endpoint")
--            , ("home", "/")
--            , ("roll", "/roll")
--            ] :: [(Text, Text)])

    -- Perform a simple roll
    get "/roll" $ do
        -- TODO: error handling
        -- http://hackage.haskell.org/package/scotty-0.7.3/docs/Web-Scotty.html#v:rescue
        --   json $ M.fromList (
        --   [ ("description", "Scheme Dice Roll")
        --   , ("input", "")
        --   , ("error", "No source provided")
        --   , ("output", "")
        --   ] :: [(Text, Text)])
        src <- param "src"

        -- Run the scheme interpreter here in runST & runRand then return the result
        gen <- liftIO newStdGen

        -- Load the stdlib
        stdlib <- liftIO $ T.readFile (T.unpack path)

        let roll = runST $ runExpr stdlib src gen

        -- TODO: debug
--      liftIO $ putStrLn $ T.unpack roll

        json $ M.fromList (
              [ ("description", "Scheme Dice Roll")
              , ("input", src)
              , ("error", "")
              , ("output", roll)
              ] :: [(Text, Text)])


--
-- NSQ Handlers
--
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
                scottyApp (scottyApplication "src/stdlib.scm") >>= run 8080
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
--  * Define/finish the json schema for nsq endpoint
--      - Look into sharing the common data between the wai and nsq endpoints
--      - Reuse the json infra between the wai and nsq endpoint but nsq "takes a json of params" and wai "takes named args in the url"
--  - Limit size of incoming program 8KB maybe enough
--  - Cap cpu and memory consumption
--  - Tweak alloc to reduce, 8KB data yields 3GB/s allocs
--  - Consider migration to attoparsec for performance (Need benchmarks first)
