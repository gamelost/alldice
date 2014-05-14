{-# LANGUAGE OverloadedStrings #-}
module Scheme.Env
    ( isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    ) where

import Control.Monad
import Control.Monad.Error
import Data.IORef
import Data.Maybe
import qualified Data.Text as T

import Scheme.Types


isBound :: LispEnv -> T.Text -> IO Bool
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: LispEnv -> T.Text -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: LispEnv -> T.Text -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: LispEnv -> T.Text -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: LispEnv -> [(T.Text, LispVal)] -> IO LispEnv
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
