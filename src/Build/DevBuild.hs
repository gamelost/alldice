{-# LANGUAGE OverloadedStrings #-}
module Build.DevBuild
    ( launchDevBuild
    ) where

import Text.Sass
import Development.Shake
import System.Process
import Control.Concurrent
import Control.Concurrent.Async
import System.Log.Logger
import System.IO


launchDevBuild :: IO ()
launchDevBuild = forkIO (concurrently elmReactor shakeBuild >> return ()) >> return ()


elmReactor :: IO ()
elmReactor = do
    noticeM "AllDice.DevBuild" "Launching Elm Reactor"
    (_, Just hout, Just herr, pid) <-
        createProcess (shell "elm-reactor --address=0.0.0.0 --port=8000")
            { std_out = CreatePipe
            , std_err = CreatePipe
            }

    stdoutListener <- async $ listenHandle NOTICE hout
    stderrListener <- async $ listenHandle ERROR herr
    waitProcess <- async (waitForProcess pid >> return ())

    _ <- waitAnyCancel [stdoutListener, stderrListener, waitProcess]
    return ()


listenHandle :: Priority -> Handle -> IO ()
listenHandle p h = do
    input <- hGetLine h
    logM "AllDice.DevBuild" p input
    listenHandle p h


shakeBuild :: IO ()
shakeBuild = do
    noticeM "AllDice.DevBuild" "Launching Shake Builder"

    threadDelay 10000000000000000
    shakeBuild
