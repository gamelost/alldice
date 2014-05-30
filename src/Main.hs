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

-- Ekg monitoring
import System.Remote.Monitoring

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
            let roll = runST $ runExpr val gen

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
runExpr :: T.Text -> StdGen -> ST s T.Text
runExpr val gen = do
    env <- primitiveBindings

    -- Inject a val
    env' <- bindVars env [("stdRngGen", Random gen)]

    -- TODO: a nicer way to inject the stdlib into the env
    -- TODO: add error reporting for invalid/bad stdlib
    mapM (evalString env') stdlib

    -- Run the scheme program given
    evalString env' val

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
    putStrLn "Starting ekg monitoring on port 8081"
    forkServer "localhost" 8081

    putStrLn "Starting server on port 8080"
    toWaiApp application >>= run 8080

-- TODO:
--  - Limit size of incoming program 8KB maybe enough
--  - Cap cpu and memory consumption
--  - Tweak alloc to reduce, 8KB data yields 3GB/s allocs
--  - Consider migration to attoparsec for performance (Need benchmarks first)




-----------------------------------------
--eval env (List [Atom "load", String filename]) =
--     load filename >>= liftM last . mapM (eval env)
--
--load :: T.Text -> IOThrowsError [LispVal]
--load filename = liftIO (T.readFile $ T.unpack filename) >>= liftThrows . readExprList
--
--runOne :: [T.Text] -> IO ()
--runOne args = do
--    file <- (T.readFile $ T.unpack (head args)) >>= return . readExprList
--    case file of
--        Left err  -> T.hPutStrLn stderr (T.pack $ show err)
--        Right val -> do
--            env <- stToIO primitiveBindings >>= \env -> stToIO (bindVars env [("args", List $ map String $ drop 1 args)])
--            stToIO (mapM (eval env) val) >>= T.hPutStrLn stderr . T.pack . show
-----------------------------------------

stdlib :: [T.Text]
stdlib =
    [ "(define (not x)            (if x #f #t))"
    , "(define (null? obj)        (if (eqv? obj '()) #t #f))"
    , "(define (list . objs)       objs)"
    , "(define (id obj)           obj)"
    , "(define (flip func)        (lambda (arg1 arg2) (func arg2 arg1)))"
    , "(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))"
    , "(define (compose f g)      (lambda (arg) (f (apply g arg))))"
    , "(define zero?              (curry = 0))"
    , "(define positive?          (curry < 0))"
    , "(define negative?          (curry > 0))"
    , "(define (odd? num)         (= (mod num 2) 1))"
    , "(define (even? num)        (= (mod num 2) 0))"
    , "(define (foldr func end lst) (if (null? lst) end (func (car lst) (foldr func end (cdr lst)))))"
    , "(define (foldl func accum lst) (if (null? lst) accum (foldl func (func accum (car lst)) (cdr lst))))"
    , "(define fold foldl)"
    , "(define reduce foldr)"
    , "(define (unfold func init pred) (if (pred init) (cons init '()) (cons init (unfold func (func init) pred))))"
    , "(define (sum . lst)         (fold + 0 lst))"
    , "(define (product . lst)     (fold * 1 lst))"
    , "(define (and . lst)         (fold && #t lst))"
    , "(define (or . lst)          (fold || #f lst))"
    , "(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))"
    , "(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))"
    , "(define (length lst)        (fold (lambda (x y) (+ x 1)) 0 lst))"
    , "(define (reverse lst)       (fold (flip cons) '() lst))"
    , "(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))"
    , "(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))"
    , "(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))"
    , "(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) #f lst))"
    , "(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))"
    , "(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))"
    , "(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) #f alist))"
    , "(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst))"
    , "(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))"

    -- Dice based stdlib functions
    , "(define (dice num) (randInt num))"
    ]
