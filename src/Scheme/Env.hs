{-# LANGUAGE OverloadedStrings, FlexibleContexts, Rank2Types, ImpredicativeTypes #-}
module Scheme.Env
    ( nullEnv
    , isBound
    , getVar
    , setVar
    , defineVar
    , bindVars
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.STRef
import qualified Data.Text as T

import Scheme.Types

nullEnv :: ST s (LispEnv s)
nullEnv = newSTRef []

isBound :: LispEnv s -> T.Text -> ST s Bool
isBound envRef var = liftM (isJust . lookup var) (readSTRef envRef)

getVar :: LispEnv s -> T.Text -> ST s (ThrowsError (LispVal s))
getVar envRef var = do
    env <- readSTRef envRef
    case lookup var env of
        Nothing  -> return $ Left $ UnboundVar "Getting an unbound variable" var
        Just val -> liftM Right (readSTRef val)

setVar :: LispEnv s -> T.Text -> LispVal s -> ST s (ThrowsError (LispVal s))
setVar envRef var value = do
    env <- readSTRef envRef
    case lookup var env of
        Nothing  -> return $ Left $ UnboundVar "Setting an unbound variable" var
        Just val -> writeSTRef val value >> return (Right value)

defineVar :: LispEnv s -> T.Text -> LispVal s -> ST s (ThrowsError (LispVal s))
defineVar envRef var value = do
     alreadyDefined <- isBound envRef var
     if alreadyDefined
        then setVar envRef var value
        else do
             valueRef <- newSTRef value
             env <- readSTRef envRef
             writeSTRef envRef ((var, valueRef) : env)
             return $ Right value

bindVars :: LispEnv s -> [(T.Text, LispVal s)] -> ST s (LispEnv s)
bindVars envRef bindings = readSTRef envRef >>= extendEnv bindings >>= newSTRef
    where
        extendEnv bindings' env = liftM (++ env) (mapM addBinding bindings')
        addBinding (var, value) = do
            ref <- newSTRef value
            return (var, ref)
