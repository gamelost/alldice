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


-- Scheme AST
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String -- TODO: do we need this
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: LispEnv}
             | IOFunc ([LispVal] -> IOThrowsError LispVal) -- TODO: don't need I/O enabled functions
             | Port Handle -- TODO: nor do we need I/O in/out probably

instance Show LispVal where
    show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


-- Scheme Error reporting
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show = showError

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


-- Scheme Execution Environment
-- TODO: Since we don't need I/O in the real app consider the ST monad
type LispEnv = IORef [(String, IORef LispVal)]

nullEnv :: IO LispEnv
nullEnv = newIORef []


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


-- TODO: not sure this is best spot
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- TODO: not sure this is best spot
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

trapError action = catchError action (return . show)

-- TODO: incomplete
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
