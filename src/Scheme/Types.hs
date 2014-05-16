{-# LANGUAGE OverloadedStrings #-}
module Scheme.Types
    ( LispVal(..)
    , LispError(..)
    , ThrowsError
    , IOThrowsError
    , LispEnv()
    , nullEnv

    , liftThrows
    , runIOThrows
    ) where

import Control.Monad
import Control.Monad.Error
import System.IO
import Text.Parsec (ParseError)
import qualified Data.Text as T

import Control.Monad.ST
import Data.STRef
import Data.IORef


-- Scheme AST
data LispVal s = Atom T.Text
               | List [LispVal s]
               | DottedList [LispVal s] (LispVal s)
               | Number Integer
               | String T.Text -- TODO: do we need this
               | Bool Bool
               | PrimitiveFunc ([LispVal s] -> ThrowsError s (LispVal s))
               | Func {params :: [T.Text], vararg :: Maybe T.Text, body :: [LispVal s], closure :: LispEnv s}
               | IOFunc ([LispVal s] -> IOThrowsError s (LispVal s)) -- TODO: don't need I/O enabled functions

instance Show (LispVal s) where
    show = T.unpack . showVal

showVal :: LispVal s -> T.Text
showVal (String contents) = T.concat ["\"", contents, "\""]
showVal (Atom name) = name
showVal (Number contents) = T.pack $ show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = T.concat ["(", unwordsList contents, ")"]
showVal (DottedList head tail) = T.concat ["(", unwordsList head, " . ", showVal tail, ")"]
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = T.concat
    [ "(lambda ("
    , T.unwords (map (T.pack . show) args)
    , case varargs of
        Nothing -> ""
        Just arg -> T.concat [" . ", arg]
    , ") ...)"
    ]
showVal (IOFunc _) = "<IO primitive>"


-- Scheme Error reporting
data LispError s = NumArgs Integer [LispVal s]
                 | TypeMismatch T.Text (LispVal s)
                 | Parser ParseError
                 | BadSpecialForm T.Text (LispVal s)
                 | NotFunction T.Text T.Text
                 | UnboundVar T.Text T.Text
                 | Default T.Text

instance Show (LispError s) where
    show = T.unpack . showError

instance Error (LispError s) where
    noMsg = Default "An error has occurred"
    strMsg = Default . T.pack

type ThrowsError s = Either (LispError s)
type IOThrowsError s = ErrorT (LispError s) IO

showError :: LispError s -> T.Text
showError (UnboundVar message varname)  = T.concat [message, ": ", varname]
showError (BadSpecialForm message form) = T.concat [message, ": ", showVal form]
showError (NotFunction message func)    = T.concat [message, ": ", func]
showError (NumArgs expected found)      = T.concat ["Expected ", (T.pack . show) expected, " args; found values ", unwordsList found]
showError (TypeMismatch expected found) = T.concat ["Invalid type: expected ", expected, ", found ", showVal found]
showError (Parser parseErr)             = T.concat ["Parse error at ", (T.pack . show) parseErr]


-- Scheme Execution Environment
type LispEnv s = STRef s [(T.Text, STRef s (LispVal s))]

nullEnv :: ST s (LispEnv s)
nullEnv = newSTRef []


unwordsList :: [LispVal s] -> T.Text
unwordsList = T.unwords . map showVal


-- TODO: not sure this is best spot
liftThrows :: ThrowsError s a -> IOThrowsError s a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- TODO: not sure this is best spot, update to toss Text
runIOThrows :: IOThrowsError s T.Text -> IO T.Text
runIOThrows action = liftM extractValue $ runErrorT (trapError action)

trapError :: (Show e, MonadError e m) => m T.Text -> m T.Text
trapError action = catchError action (return . T.pack . show)

-- TODO: incomplete
extractValue :: ThrowsError s a -> a
extractValue (Right val) = val
