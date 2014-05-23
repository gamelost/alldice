{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans

import Data.Aeson hiding (json, String, Number)
import Data.IORef
import Data.STRef
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

-- Not ideal but should in theory work for now
import System.Random

import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Primitives

-- The Site argument
type SchemeRandom = StdGen
data MyRoute = MyRoute (IORef SchemeRandom)

-- Make MyRoute Routable
mkRoute "MyRoute" [parseRoutes|
/             HomeR           GET
/roll         RollR           GET
|]


-- Handlers

-- Util: Fetch the rng gen
getRng :: HandlerM MyRoute SchemeRandom
getRng = do
  MyRoute dbref <- master
  liftIO $ readIORef dbref

setRng :: SchemeRandom -> HandlerM MyRoute ()
setRng rng = do
  MyRoute dbref <- master
  liftIO $ writeIORef dbref rng

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
        src = liftM T.decodeUtf8 $ join $ lookup "src" query :: Maybe Text

    case src of
        Nothing -> json $ M.fromList (
                      [ ("description", "Scheme Dice Roll")
                      , ("input", "")
                      , ("error", "No source provided")
                      , ("output", "")
                      ] :: [(Text, Text)])

        Just val -> do
            -- Run the scheme interpreter here in runST & runRand then return the result
            let roll = runST $ runExpr val 1 -- TODO: dummy val
--            rng <- getRng
--            let roll = (T.pack . show) $ evalRand (runST $ runExprRand val) rng
--            setRng rng

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

-- TODO: extract the seed out maybe
runExpr :: T.Text -> Integer -> ST s T.Text
runExpr val seed = do
    env <- primitiveBindings

    -- Inject a val
    env' <- setVar env "stdRngGen" (Number seed)

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
    gen <- liftIO $ getStdGen
    db <- liftIO $ newIORef gen
    middleware logStdoutDev
    route (MyRoute db)
    defaultAction $ staticApp $ defaultFileServerSettings "static"

-- Run the application
main :: IO ()
main = do
    putStrLn "Starting server on port 8080"
    toWaiApp application >>= run 8080
