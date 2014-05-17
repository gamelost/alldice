{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Scheme.Env
    ( isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.ST
import Data.Maybe
import Data.STRef
import qualified Data.Text as T

import Scheme.Types

isBound :: LispEnv s -> T.Text -> ST s Bool
isBound envRef var = liftM (isJust . lookup var) (readSTRef envRef)

-- TODO: not sure the type is correct but we'll see
getVar :: MonadError (LispError s1) (ST s) => LispEnv s -> T.Text -> ST s (LispVal s)
getVar envRef var = do
    env <- readSTRef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (readSTRef)
          (lookup var env)

setVar :: MonadError (LispError s1) (ST s) => LispEnv s -> T.Text -> LispVal s -> ST s (LispVal s)
setVar envRef var value = do
    env <- readSTRef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (flip writeSTRef value)
          (lookup var env)
    return value

defineVar :: MonadError (LispError s1) (ST s) => LispEnv s -> T.Text -> LispVal s -> ST s (LispVal s)
defineVar envRef var value = do
     alreadyDefined <- isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else do
             valueRef <- newSTRef value
             env <- readSTRef envRef
             writeSTRef envRef ((var, valueRef) : env)
             return value

bindVars :: LispEnv s -> [(T.Text, LispVal s)] -> ST s (LispEnv s)
bindVars envRef bindings = readSTRef envRef >>= extendEnv bindings >>= newSTRef
    where
        extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
            ref <- newSTRef value
            return (var, ref)
