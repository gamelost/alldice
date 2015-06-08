{-# LANGUAGE OverloadedStrings #-}
module AllDice.Web
    ( scottyApplication
    ) where

import Control.Monad.ST
import Control.Monad.Trans

import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Web.Scotty
import Network.Wai.Middleware.RequestLogger

-- Not ideal but should in theory work for now
import System.Random

-- Scheme interpreter
import AllDice.Scheme

scottyApplication :: T.Text -> ScottyM ()
scottyApplication path = do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    -- Home
    get "/" $ file "psrc/index.html"
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
