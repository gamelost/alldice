{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Scheme.Primitives
    ( primitives
    ) where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Scheme.Types

-- TODO: add support for other types test (symbol? string? number? etc)
-- TODO: add support for symbol handling (symbol ~= atom)
primitives :: [(T.Text, [LispVal s] -> ThrowsError (LispVal s))]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal s] -> ThrowsError (LispVal s)
numericBinop op           []  = Left $ NumArgs 2 []
numericBinop op singleVal@[_] = Left $ NumArgs 2 (map expand singleVal)
numericBinop op params        = liftM (Number . foldl1 op) (mapM unpackNum params)

-- TODO: consider having it return 0 if not valid number
unpackNum :: LispVal s -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    case T.decimal n of
        Left _       -> Left $ TypeMismatch "number" $ (expand $ String n)
        Right (i, t) -> if T.null t
                        then return i
                        else Left $ TypeMismatch "Non decimal value found" $ (expand $ String n)
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = Left $ TypeMismatch "number" (expand notNum)


boolBinop :: (LispVal s -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal s] -> ThrowsError (LispVal s)
boolBinop unpacker op args =
    if length args /= 2
    then Left $ NumArgs 2 (map expand args)
    else do
        left <- unpacker $ head args
        right <- unpacker $ last args
        return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal s] -> ThrowsError (LispVal s)
numBoolBinop  = boolBinop unpackNum

strBoolBinop :: (T.Text -> T.Text -> Bool) -> [LispVal s] -> ThrowsError (LispVal s)
strBoolBinop  = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal s] -> ThrowsError (LispVal s)
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal s -> ThrowsError T.Text
unpackStr (String s) = return s
unpackStr (Number s) = return $ T.pack $ show s
unpackStr (Bool s)   = return $ T.pack $ show s
unpackStr notString  = Left $ TypeMismatch "string" (expand notString)

unpackBool :: LispVal s -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = Left $ TypeMismatch "boolean" (expand notBool)

car :: [LispVal s] -> ThrowsError (LispVal s)
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = Left $ TypeMismatch "pair" (expand badArg)
car badArgList              = Left $ NumArgs 1 (map expand badArgList)

cdr :: [LispVal s] -> ThrowsError (LispVal s)
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = Left $ TypeMismatch "pair" (expand badArg)
cdr badArgList              = Left $ NumArgs 1 (map expand badArgList)

cons :: [LispVal s] -> ThrowsError (LispVal s)
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = Left $ NumArgs 2 (map expand badArgList)

eqv :: [LispVal s] -> ThrowsError (LispVal s)
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                -- TODO: non-exhaustive matches
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = Left $ NumArgs 2 (map expand badArgList)


-- TODO: This is weak typing, may want to do away with it (non-standard)
data Unpacker s = forall a. Eq a => AnyUnpacker (LispVal s -> ThrowsError a)

unpackEquals :: LispVal s -> LispVal s -> Unpacker s -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    Right $ either
            (const False)
            (id)
            (do
             unpacked1 <- unpacker arg1
             unpacked2 <- unpacker arg2
             return $ unpacked1 == unpacked2
            )

-- TODO: Introducts a bug here, fix it
-- TODO: equal? has a bug in that a list of values is compared using eqv?
-- instead of equal?. For example, (equal? '(1 "2") '(1 2)) = #f, while
-- you'd expect it to be #t. Change equal? so that it continues to ignore
-- types as it recurses into list structures. You can either do this
-- explicitly, following the example in eqv?, or factor the list clause
-- into a separate helper function that is parameterized by the equality
-- testing function.
equal :: [LispVal s] -> ThrowsError (LispVal s)
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = Left $ NumArgs 2 (map expand badArgList)
