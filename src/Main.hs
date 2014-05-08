{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
module Main where

import Network.Wai
import Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.ContentTypes
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson hiding (json)
import Data.IORef
import qualified Data.Map as M
import Control.Monad.Trans
import Network.Wai.Middleware.RequestLogger

import System.Random.MWC

-- The Site argument
type Rand = GenIO
data MyRoute = MyRoute (IORef Rand)

-- Make MyRoute Routable
mkRoute "MyRoute" [parseRoutes|
/             HomeR           GET
/roll         RollR           GET
|]


-- Handlers

-- Util: Fetch the database
getDB :: HandlerM MyRoute Rand
getDB = do
  MyRoute dbref <- master
  liftIO $ readIORef dbref

-- Display the possible actions
getHomeR :: Handler MyRoute
getHomeR = runHandlerM $ do
  json $ M.fromList (
                 [("description", [["Simple Dice Roller Example"]])
                 ,("links"
                  ,[["home", showRoute HomeR]
                   ,["roll", showRoute RollR]
                   ]
                  )
                 ] :: [(Text, [[Text]])] )


-- Perform a simple roll
getRollR :: Handler MyRoute
getRollR = runHandlerM $ do
  db <- getDB
  -- TODO: roll some kicking rad dices

  json $ M.fromList (
          [("description", [["Dice Rolls"]])
          ,("roll", [["6 - chosen by fair dice"]])] :: [(Text, [[Text]])] )


-- The application that uses our route
-- NOTE: We use the Route Monad to simplify routing
application :: RouteM ()
application = do
    gen <- liftIO $ createSystemRandom
    db <- liftIO $ newIORef gen
    middleware logStdoutDev
    route (MyRoute db)
    defaultAction $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = do
  putStrLn "Starting server on port 8080"
  toWaiApp application >>= run 8080
