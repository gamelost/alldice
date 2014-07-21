{-# LANGUAGE OverloadedStrings #-}
module AllDice.Scheme
    ( runExpr
    ) where

import Control.Monad.ST
import qualified Data.Text as T

-- Not ideal but should in theory work for now
import System.Random

-- Scheme interpreter
import Scheme.Types
import Scheme.Env
import Scheme.Parser
import Scheme.Evaluator


evalString :: LispEnv s -> T.Text -> ST s T.Text
evalString env expr = do
    eexpr <- evalExpr env expr
    case eexpr of
        Left err  -> return $ (T.pack . show) err
        Right val -> return $ (T.pack . show) val

evalExpr :: LispEnv s -> T.Text -> ST s (ThrowsError (LispVal s))
evalExpr env expr =
    case readExpr expr of
        Left err  -> return $ Left err
        Right val -> eval env val

evalFile :: LispEnv s -> T.Text -> ST s T.Text
evalFile env expr = do
    exps <- evalExprList env expr
    T.unlines `fmap` mapM (\eexp -> case eexp of
        Left err  -> return $ (T.pack . show) err
        Right val -> return $ (T.pack . show) val) exps

evalExprList :: LispEnv s -> T.Text -> ST s [ThrowsError (LispVal s)]
evalExprList env expr =
    case readExprList expr of
        Left err  -> return [Left err]
        Right val -> mapM (eval env) val

runExpr :: T.Text -> T.Text -> StdGen -> ST s T.Text
runExpr stdlib val gen = do
    env <- primitiveBindings

    -- Inject a val
    env' <- bindVars env [("stdRngGen", Random gen)]

    -- TODO: a nicer way to inject the stdlib into the env
    -- TODO: add error reporting for invalid/bad stdlib
    _ <- evalFile env' stdlib

    -- Run the scheme program given
    evalString env' val
