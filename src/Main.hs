{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent (killThread)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E

-- Web
import Web.Scotty
import Network.Wai.Handler.Warp

-- Ekg monitoring
import System.Remote.Monitoring

-- Logger
import System.IO (stderr, Handle)
import System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger, Priority(DEBUG), Priority(EMERGENCY), setLevel)
import System.Log.Handler.Simple (streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

-- Alldice endpoints
import AllDice.Web


-- TODO:
--  * Define/finish the json schema for nsq endpoint
--      - Look into sharing the common data between the wai and nsq endpoints
--      - Reuse the json infra between the wai and nsq endpoint but nsq "takes a json of params" and wai "takes named args in the url"
--  - Limit size of incoming program 8KB maybe enough
--  - Cap cpu and memory consumption
--  - Tweak alloc to reduce, 8KB data yields 3GB/s allocs
--  - Consider migration to attoparsec for performance (Need benchmarks first)
main :: IO ()
main = do
    -- Logger stuff
    logStream <- withFormatter <$> streamHandler stderr DEBUG
    let loggerName = rootLoggerName

    updateGlobalLogger loggerName (setLevel DEBUG)
    updateGlobalLogger loggerName (setHandlers [logStream])

    -- Ekg stuff
    putStrLn "Starting ekg monitoring on port 8081"
    ekgServer <- forkServer "localhost" 8081

    -- TODO: this bit is kinda gross but it lets us kill the ekg server when the wai or any other children threads dies.
    E.catch (do
                putStrLn "Starting server on port 8080"
                scottyApp (scottyApplication "src/stdlib.scm") >>= run 8080
        ) (\(E.ErrorCall e) -> (do
            putStrLn e
            killThread $ serverThreadId $ ekgServer
        ))

-- Log Formatter
withFormatter :: GenericHandler System.IO.Handle -> GenericHandler System.IO.Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"
