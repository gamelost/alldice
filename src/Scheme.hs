{-# LANGUAGE OverloadedStrings #-}
module Scheme where

import Control.Monad
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Primitives


flushStr :: T.Text -> IO ()
flushStr str = T.putStr str >> hFlush stdout

readPrompt :: T.Text -> IO T.Text
readPrompt prompt = flushStr prompt >> T.getLine

evalAndPrint :: LispEnv -> T.Text -> IO ()
evalAndPrint env expr = evalString env expr >>= T.putStrLn

evalString :: LispEnv -> T.Text -> IO T.Text
evalString env expr = runIOThrows $ liftM (T.pack . show) $ join $ liftThrows $ liftM (eval env) (readExpr expr)

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   unless (pred result) (action result >> until_ pred prompt action)

runOne :: [T.Text] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (liftM (T.pack . show) $ eval env (List [Atom "load", String (head args)]))
        >>= T.hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne (map T.pack args)
