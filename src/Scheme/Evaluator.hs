{-# LANGUAGE OverloadedStrings, FlexibleContexts, ImpredicativeTypes #-}
module Scheme.Evaluator
    ( eval
    , primitiveBindings
    , load
    ) where

import Control.Monad.ST
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Scheme.Types
import Scheme.Env
import Scheme.Parser (readExprList)
import Scheme.Primitives


-- TODO: support other like cond/case expressions
eval :: MonadError (LispError s) (ST s) => LispEnv s -> LispVal s -> ST s (LispVal s)
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = -- TODO: only accept Bool and throw on any other value
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: (Monad m, Show a) => Maybe T.Text -> LispEnv s -> [a] -> [LispVal s] -> m (LispVal s)
makeFunc varargs env params body = return $ Func (map (T.pack . show) params) varargs body env

makeNormalFunc :: MonadError (LispError s) (ST s) => LispEnv s -> [LispVal s] -> [LispVal s] -> ST s (LispVal s)
makeNormalFunc = makeFunc Nothing

makeVarargs :: MonadError (LispError s) (ST s) => LispVal s -> LispEnv s -> [LispVal s] -> [LispVal s] -> ST s (LispVal s)
makeVarargs = makeFunc . Just . T.pack . show


apply :: MonadError (LispError s) (ST s) => LispVal s -> [LispVal s] -> ST s (LispVal s)
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else envEval params varargs body closure args
    where
        num :: [a] -> Integer
        num = toInteger . length

        remainingArgs :: [a] -> [b] -> [b]
        remainingArgs params args = drop (length params) args

        evalBody :: MonadError (LispError s) (ST s) => [LispVal s] -> LispEnv s -> ST s (LispVal s)
        evalBody body env = liftM last $ mapM (eval env) body

        envEval :: MonadError (LispError s) (ST s) => [T.Text] -> Maybe T.Text -> [LispVal s] -> LispEnv s -> [LispVal s] -> ST s (LispVal s)
        envEval params varargs body closure args = do
            env  <- bindVars closure $ zip params args
            env' <- bindVarArgs params args varargs env
            evalBody body env'

        bindVarArgs :: [a] -> [LispVal s] -> Maybe T.Text -> LispEnv s -> ST s (LispEnv s)
        bindVarArgs params args arg env = case arg of
            Just argName -> bindVars env [(argName, List $ remainingArgs params args)]
            Nothing -> return env

apply (PrimitiveFunc func) args =
    case func args of
        Left err  -> throwError err
        Right val -> return val

apply (IOFunc func) args = func args


-- TODO: a bit of a hack
--eval env (List [Atom "load", String filename]) =
--     load filename >>= liftM last . mapM (eval env)
load :: T.Text -> IOThrowsError s [LispVal s]
load filename = liftIO (T.readFile $ T.unpack filename) >>= liftThrows . readExprList


-- TODO: another semi-hack this probably should be half in primitive and half not for stateful primitives
primitiveBindings :: ST s (LispEnv s)
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
     where makeFunc constructor (var, func) = (var, constructor func)

-- IO primitives
ioPrimitives :: [(T.Text, MonadError (LispError s) (ST s) => [LispVal s] -> ST s (LispVal s))]
ioPrimitives = [("apply", applyProc)]

applyProc :: MonadError (LispError s) (ST s) => [LispVal s] -> ST s (LispVal s)
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
