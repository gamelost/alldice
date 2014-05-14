{-# LANGUAGE OverloadedStrings #-}
module Scheme.Parser
    ( symbol
    , spaces
    , parseString
    , parseAtom
    , parseNumber
    , parseList
    , parseDottedList
    , parseQuoted
    , parseExpr
    , readExpr
    , readExprList
    ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Control.Monad.Error
import Text.Parsec hiding (spaces)
import Text.Parsec.Text
import qualified Data.Text as T

import Scheme.Types


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = char '"' *> (String . T.pack <$> many (noneOf "\"")) <* char '"'

-- TODO: update this to work on text
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom (T.pack atom)

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = DottedList <$> endBy parseExpr spaces <*> (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> char '(' *> (try parseList <|> parseDottedList) <* char ')'

readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> T.Text -> ThrowsError a
readOrThrow parser input = case parse parser "Scheme" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
