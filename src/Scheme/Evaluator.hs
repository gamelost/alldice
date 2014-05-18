{-# LANGUAGE OverloadedStrings, FlexibleContexts, ImpredicativeTypes #-}
module Scheme.Evaluator
    ( eval
    , primitiveBindings
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Scheme.Types
import Scheme.Env
import Scheme.Parser (readExprList)
import Scheme.Primitives


-- TODO: support other like cond/case expressions
eval :: LispEnv s -> LispVal s -> ST s (ThrowsError (LispVal s))
eval env val@(String _) = return $ Right val
eval env val@(Number _) = return $ Right val
eval env val@(Bool _)   = return $ Right val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return $ Right val

-- TODO: only accept Bool and throw on any other value
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
        Left err  -> return result
        Right val ->
            case val of
                Bool False -> eval env alt
                otherwise  -> eval env conseq

eval env (List [Atom "set!", Atom var, form]) = do
    result <- eval env form
    case result of
        Left err  -> return result
        Right val -> setVar env var val

eval env (List [Atom "define", Atom var, form]) = do
    result <- eval env form
    case result of
        Left err  -> return result
        Right val -> defineVar env var val

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
    liftM Right $ makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    liftM Right $ makeVarargs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    liftM Right $ makeVarargs varargs env [] body

eval env (List (function : args)) = do
    func <- eval env function
    case func of
        Left err   -> return func
        Right fval -> do
            argVals <- mapM (eval env) args
            -- TODO: make this better, we just grab the first error and
            -- return it up the stack instead of them all
            let (err, vals) = partitionEithers argVals

            if null err
            then apply fval vals
            else return $ Left $ head err -- TODO: unsafe head

eval env badForm = return $ Left $ BadSpecialForm "Unrecognized special form" (expand badForm)

makeFunc :: Show a => Maybe T.Text -> LispEnv s -> [a] -> [LispVal s] -> ST s (LispVal s)
makeFunc varargs env params body = return $ Func (map (T.pack . show) params) varargs body env

makeNormalFunc :: LispEnv s -> [LispVal s] -> [LispVal s] -> ST s (LispVal s)
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal s -> LispEnv s -> [LispVal s] -> [LispVal s] -> ST s (LispVal s)
makeVarargs = makeFunc . Just . T.pack . show


apply :: LispVal s -> [LispVal s] -> ST s (ThrowsError (LispVal s))
apply (PrimitiveFunc func) args = return $ func args
apply (StatefulFunc func) args = func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
    then return $ Left $ NumArgs (num params) (map expand args)
    else envEval params varargs body closure args

num :: [a] -> Integer
num = toInteger . length

remainingArgs :: [a] -> [b] -> [b]
remainingArgs params args = drop (length params) args

evalBody :: [LispVal s] -> LispEnv s -> ST s (ThrowsError (LispVal s))
evalBody body env = liftM last $ mapM (eval env) body

envEval :: [T.Text] -> Maybe T.Text -> [LispVal s] -> LispEnv s -> [LispVal s] -> ST s (ThrowsError (LispVal s))
envEval params varargs body closure args = do
    env  <- bindVars closure $ zip params args
    env' <- bindVarArgs params args varargs env
    evalBody body env'

bindVarArgs :: [a] -> [LispVal s] -> Maybe T.Text -> LispEnv s -> ST s (LispEnv s)
bindVarArgs params args arg env = case arg of
    Just argName -> bindVars env [(argName, List $ remainingArgs params args)]
    Nothing -> return env

-- TODO: another semi-hack this probably should be half in primitive and half not for stateful primitives
primitiveBindings :: ST s (LispEnv s)
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc PrimitiveFunc) primitives ++ map (makeFunc StatefulFunc) ioPrimitives)
     where makeFunc constructor (var, func) = (var, constructor func)

-- IO primitives
ioPrimitives :: [(T.Text, [LispVal s] -> ST s (ThrowsError (LispVal s)))]
ioPrimitives = [("apply", applyProc)]

applyProc :: [LispVal s] -> ST s (ThrowsError (LispVal s))
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
