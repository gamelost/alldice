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
import Safe

-- Not ideal but should in theory work for now
import System.Random

import Scheme.Types
import Scheme.Env
import Scheme.Primitives

import Debug.Trace

-- TODO: support other like cond/case expressions
eval :: LispEnv s -> LispVal s -> ST s (ThrowsError (LispVal s))
eval _ val@(String _) = return $ Right val
eval _ val@(Number _) = return $ Right val
eval _ val@(Bool _)   = return $ Right val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return $ Right val

-- TODO: only accept Bool and throw on any other value
eval env (List [Atom "if", pred', conseq, alt]) = do
    result <- eval env pred'
    case result of
        Left _  -> return result
        Right val ->
            case val of
                Bool False -> eval env alt
                _          -> eval env conseq

eval env (List [Atom "set!", Atom var, form]) = do
    result <- eval env form
    case result of
        Left _  -> return result
        Right val -> setVar env var val

eval env (List [Atom "define", Atom var, form]) = do
    result <- eval env form
    case result of
        Left _  -> return result
        Right val -> defineVar env var val

eval env (List (Atom "define" : List (Atom var : params') : body')) =
    makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
    makeVarargs varargs env params' body' >>= defineVar env var

eval env (List (Atom "lambda" : List params' : body')) =
    liftM Right $ makeNormalFunc env params' body'

eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
    liftM Right $ makeVarargs varargs env params' body'

eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
    liftM Right $ makeVarargs varargs env [] body'

-- TODO: abstract this out of the eval better later on
eval env (List (Atom "randInt" : val)) = randIntProc env val

eval env (List (function : args)) = do
    func <- eval env function
    case func of
        Left _   -> return func
        Right fval -> do
            argVals <- mapM (eval env) args
            -- TODO: make this better, we just grab the first error and
            -- return it up the stack instead of them all
            let (err, vals) = partitionEithers argVals

            if null err
            then apply fval vals
            else return $ Left $ headDef (Default "eval safehead") err

eval _ badForm = return $ Left $ BadSpecialForm "Unrecognized special form" (expand badForm)

makeFunc :: Show a => Maybe T.Text -> LispEnv s -> [a] -> [LispVal s] -> ST s (LispVal s)
makeFunc varargs env params' body' = return $ Func (map (T.pack . show) params') varargs body' env

makeNormalFunc :: LispEnv s -> [LispVal s] -> [LispVal s] -> ST s (LispVal s)
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal s -> LispEnv s -> [LispVal s] -> [LispVal s] -> ST s (LispVal s)
makeVarargs = makeFunc . Just . T.pack . show


apply :: LispVal s -> [LispVal s] -> ST s (ThrowsError (LispVal s))
apply (PrimitiveFunc func) args = return $ func args
apply (StatefulFunc func) args = func args
apply (Func params' varargs body' closure') args =
    if num params' /= num args && isNothing varargs
    then return $ Left $ NumArgs (num params') (map expand args)
    else envEval params' varargs body' closure' args

-- TODO: this doesn't seem right
apply var (PrimitiveFunc func : args) = return $ func (var : args)

-- Fallthrough - debugging
-- TODO: clean up the trace
apply func args = traceShow ("apply-debug:" :: String, func, args) $ return $ Right func


num :: [a] -> Integer
num = toInteger . length

remainingArgs :: [a] -> [b] -> [b]
remainingArgs params' = drop (length params')

evalBody :: [LispVal s] -> LispEnv s -> ST s (ThrowsError (LispVal s))
evalBody body' env = liftM (lastDef (Left $ Default "evalBody - last")) $ mapM (eval env) body'

envEval :: [T.Text] -> Maybe T.Text -> [LispVal s] -> LispEnv s -> [LispVal s] -> ST s (ThrowsError (LispVal s))
envEval params' varargs body' closure' args = do
    env  <- bindVars closure' $ zip params' args
    env' <- bindVarArgs params' args varargs env
    evalBody body' env'

bindVarArgs :: [a] -> [LispVal s] -> Maybe T.Text -> LispEnv s -> ST s (LispEnv s)
bindVarArgs params' args arg env = case arg of
    Just argName -> bindVars env [(argName, List $ remainingArgs params' args)]
    Nothing -> return env

-- TODO: another semi-hack this probably should be half in primitive and half not for stateful primitives
primitiveBindings :: ST s (LispEnv s)
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc' PrimitiveFunc) primitives ++ map (makeFunc' StatefulFunc) closurePrimitives)
     where makeFunc' constructor (var, func) = (var, constructor func)

-- Primitives that closes over some nested environment
closurePrimitives :: [(T.Text, [LispVal s] -> ST s (ThrowsError (LispVal s)))]
closurePrimitives = [ ("apply", applyProc) ]

-- TODO: need to pattern match []
applyProc :: [LispVal s] -> ST s (ThrowsError (LispVal s))
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc []                = return $ Left $ Default "applyProc booper"

-- TODO: add in support for handling a list vs 2 integers
randIntProc :: LispEnv s -> [LispVal s] -> ST s (ThrowsError (LispVal s))

-- TODO: custom handling to handle being handed an "variable" instead of
-- a hardcoded number to support reuse in define
randIntProc env [Atom n]   = do
    var <- getVar env n
    case var of
        Left err  -> return $ Left err
        Right val -> randIntProc env [val]

randIntProc env [Number n] = do
    gen <- getVar env "stdRngGen"
    case gen of
        Left err -> return $ Left err
        Right (Random gen') -> do
            let (val, gen'') = randomR (1, n) gen'
            _ <- setVar env "stdRngGen" (Random gen'')

            return $ Right $ Number val
        -- TODO: non-exhaustive pattern matches
        Right _ -> return $ Left $ Default "randIntProc booper"

randIntProc _ [badArg]          = return $ Left $ TypeMismatch "Number" (expand badArg)
randIntProc _ badArgList        = return $ Left $ NumArgs 1 (map expand badArgList)
