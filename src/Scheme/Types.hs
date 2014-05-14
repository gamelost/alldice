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
import Data.IORef
import System.IO
import Text.Parsec (ParseError)
import qualified Data.Text as T


-- Scheme AST
data LispVal = Atom T.Text
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String T.Text -- TODO: do we need this
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [T.Text], vararg :: Maybe T.Text, body :: [LispVal], closure :: LispEnv}
             | IOFunc ([LispVal] -> IOThrowsError LispVal) -- TODO: don't need I/O enabled functions
             | Port Handle -- TODO: nor do we need I/O in/out probably

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
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
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


-- Scheme Error reporting
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch T.Text LispVal
               | Parser ParseError
               | BadSpecialForm T.Text LispVal
               | NotFunction T.Text T.Text
               | UnboundVar T.Text T.Text
               | Default T.Text

instance Show LispError where
    show = T.unpack . showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default . T.pack

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

showError :: LispError -> T.Text
showError (UnboundVar message varname)  = T.concat [message, ": ", varname]
showError (BadSpecialForm message form) = T.concat [message, ": ", showVal form]
showError (NotFunction message func)    = T.concat [message, ": ", func]
showError (NumArgs expected found)      = T.concat ["Expected ", (T.pack . show) expected, " args; found values ", unwordsList found]
showError (TypeMismatch expected found) = T.concat ["Invalid type: expected ", expected, ", found ", showVal found]
showError (Parser parseErr)             = T.concat ["Parse error at ", (T.pack . show) parseErr]


-- Scheme Execution Environment
-- TODO: Since we don't need I/O in the real app consider the ST monad
type LispEnv = IORef [(T.Text, IORef LispVal)]

nullEnv :: IO LispEnv
nullEnv = newIORef []


unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . map showVal


-- TODO: not sure this is best spot
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- TODO: not sure this is best spot, update to toss Text
runIOThrows :: IOThrowsError T.Text -> IO T.Text
runIOThrows action = liftM extractValue $ runErrorT (trapError action)

trapError action = catchError action (return . T.pack . show)

-- TODO: incomplete
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
