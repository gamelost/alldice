{-# LANGUAGE OverloadedStrings #-}
module Scheme.Evaluator
    ( eval

    -- TODO: this should be internal in theory
    , apply
    ) where

import Control.Monad.Error
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Scheme.Types
import Scheme.Env
import Scheme.Parser (readExprList)


-- TODO: support other like cond/case expressions
eval :: LispEnv -> LispVal -> IOThrowsError LispVal
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
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: (Monad m, Show a) => Maybe T.Text -> LispEnv -> [a] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map (T.pack . show) params) varargs body env

makeNormalFunc :: LispEnv -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> LispEnv -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . T.pack . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs
       then throwError $ NumArgs (num params) args
       else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
              Nothing -> return env
apply (IOFunc func) args = func args


-- TODO: a bit of a hack
load :: T.Text -> IOThrowsError [LispVal]
load filename = liftIO (T.readFile $ T.unpack filename) >>= liftThrows . readExprList
