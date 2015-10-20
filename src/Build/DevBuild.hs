{-# LANGUAGE OverloadedStrings #-}
module Build.DevBuild
    ( launchDevBuild
    ) where

import Text.Sass
import System.Process
import Control.Concurrent
import Control.Concurrent.Async
import System.Log.Logger
import System.IO
import System.FSNotify
import System.Directory
import Control.Monad (forever, void)
import System.FilePath
import Data.List (isSuffixOf)
import qualified Data.ByteString as BS


launchDevBuild :: IO ()
launchDevBuild = forkIO (concurrently elmReactor builder >> return ()) >> return ()


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


builder :: IO ()
builder = do
    noticeM "AllDice.DevBuild" "Launching Builder"

    -- Initial setup
    createDirectoryIfMissing False "dist"
    copyFile "assets/index.html" "dist/index.html"
    compileSASS "assets/sass/style.scss" "dist/style.css"

    withManager $ \w -> do
        void $ watchTree w "assets" (const True) (\e -> do
                copyIfChanged e "index.html" "dist"
                rebuildSASSIfChanged e "scss" "dist"
            )
        forever $ threadDelay 1000000


copyIfChanged :: Event -> String -> FilePath -> IO ()
copyIfChanged (Added fp _) file dst
    | isSuffixOf file fp    = noticeM "AllDice.DevBuild" ("Copied: " ++ file) >> copyFile fp (dst </> file)
    | otherwise             = return ()
copyIfChanged (Modified fp _) file dst
    | isSuffixOf file fp    = noticeM "AllDice.DevBuild" ("Copied: " ++ file) >> copyFile fp (dst </> file)
    | otherwise             = return ()
copyIfChanged (Removed fp _) file _
    | isSuffixOf file fp    = errorM "AllDice.DevBuild" ("The " ++ file ++ " got deleted!")
    | otherwise             = return ()


rebuildSASSIfChanged :: Event -> String -> FilePath -> IO ()
rebuildSASSIfChanged (Added fp _) file dst
    | isSuffixOf file fp    = compileSASS "assets/sass/style.scss" (dst </> "style.css")
    | otherwise             = return ()
rebuildSASSIfChanged (Modified fp _) file dst
    | isSuffixOf file fp    = compileSASS "assets/sass/style.scss" (dst </> "style.css")
    | otherwise             = return ()
rebuildSASSIfChanged (Removed fp _) file dst
    | isSuffixOf file fp    = compileSASS "assets/sass/style.scss" (dst </> "style.css")
    | otherwise             = return ()


compileSASS :: FilePath -> FilePath -> IO ()
compileSASS i c = do
    noticeM "AllDice.DevBuild" "Rebuilding the css"

    let opt = def
            { sassPrecision = 10
            , sassOutputStyle = SassStyleExpanded
            , sassSourceComments = True
            , sassIncludePaths = Just ["libs/bootstrap-sass/assets/stylesheets"]
            }
    sass <- compileFile i opt

    case sass of
        Left e  -> errorM "AllDice.DevBuild" ("libsass error: " ++ show e)
        Right x -> BS.writeFile c (resultString x) >> noticeM "AllDice.DevBuild" "Built the css"
