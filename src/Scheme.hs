{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses #-}
module Scheme where

import Control.Monad
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Monad.ST
import Data.STRef

import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator
import Scheme.Primitives


flushStr :: T.Text -> IO ()
flushStr str = T.putStr str >> hFlush stdout

readPrompt :: T.Text -> IO T.Text
readPrompt prompt = flushStr prompt >> T.getLine

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

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   unless (pred result) (action result >> until_ pred prompt action)

runOne :: [T.Text] -> IO ()
runOne args = do
    file <- (T.readFile $ T.unpack (head args)) >>= return . readExprList
    case file of
        Left err  -> T.hPutStrLn stderr (T.pack $ show err)
        Right val -> do
            env <- stToIO primitiveBindings >>= \env -> stToIO (bindVars env [("args", List $ map String $ drop 1 args)])
            stToIO (mapM (eval env) val) >>= T.hPutStrLn stderr . T.pack . show

runRepl :: IO ()
runRepl = stToIO primitiveBindings >>= \env -> until_ (== "quit") (readPrompt "Lisp>>> ") $ (\expr -> stToIO (evalString env expr) >>= T.putStrLn)

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne (map T.pack args)
