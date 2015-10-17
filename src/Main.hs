{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative

-- Web
import Web.Scotty
import Network.Wai.Handler.Warp

-- Logger
import System.IO (stderr, Handle)
import System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger, Priority(DEBUG), setLevel)
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

    putStrLn "Starting server on port 8081"
    scottyApp (scottyApplication "src/stdlib.scm") >>= run 8081

-- Log Formatter
withFormatter :: GenericHandler System.IO.Handle -> GenericHandler System.IO.Handle
withFormatter handler = setFormatter handler formatter
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"
