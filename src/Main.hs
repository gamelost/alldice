{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans

import Data.Aeson hiding (json, String)
import Data.IORef
import Data.STRef
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Routes
import Network.Wai.Middleware.Routes.ContentTypes

import System.Environment
import System.IO
import System.Random.MWC

import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Primitives

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
                 [ ("description", "URI endpoint")
                 , ("home", showRoute HomeR)
                 , ("roll", showRoute RollR)
                 ] :: [(Text, Text)])

-- Perform a simple roll
getRollR :: Handler MyRoute
getRollR = runHandlerM $ do
  db <- getDB

  -- Run the scheme interpreter here in runST then return the result
  let roll = runST $ runExpr "(+ 1 2)"

  json $ M.fromList (
          [ ("description", "Scheme Dice Roll")
          , ("input", "(+ 1 2)")
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

runExpr :: T.Text -> ST s T.Text
runExpr val = do
    env <- primitiveBindings
    evalString env val

-----------------------------------------
--runOne :: [T.Text] -> IO ()
--runOne args = do
--    file <- (T.readFile $ T.unpack (head args)) >>= return . readExprList
--    case file of
--        Left err  -> T.hPutStrLn stderr (T.pack $ show err)
--        Right val -> do
--            env <- stToIO primitiveBindings >>= \env -> stToIO (bindVars env [("args", List $ map String $ drop 1 args)])
--            stToIO (mapM (eval env) val) >>= T.hPutStrLn stderr . T.pack . show
-----------------------------------------


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
