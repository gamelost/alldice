{-# LANGUAGE OverloadedStrings, FlexibleContexts, MultiParamTypeClasses #-}
module Scheme where

import Control.Monad
import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Monad.Error
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
evalString env expr = catchError (liftM (T.pack . show) (evalExpr env expr)) (return . T.pack . show)

instance MonadError (LispError s) (ST s) where
    throwError = undefined
    catchError = undefined

evalExpr :: MonadError (LispError s) (ST s) => LispEnv s -> T.Text -> ST s (LispVal s)
evalExpr env expr =
    case readExpr expr of
        Left err  -> throwError err
        Right val -> eval env val

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   unless (pred result) (action result >> until_ pred prompt action)

runOne = undefined
--runOne :: [T.Text] -> IO ()
--runOne args = do
--    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
--    runIOThrows (liftM (T.pack . show) $ eval env (List [Atom "load", String (head args)]))
--        >>= T.hPutStrLn stderr

runRepl :: IO ()
runRepl = stToIO primitiveBindings >>= \env -> until_ (== "quit") (readPrompt "Lisp>>> ") $ (\expr -> stToIO (evalString env expr) >>= T.putStrLn)

main :: IO ()
main = do
    args <- getArgs
    if null args then runRepl else runOne (map T.pack args)
