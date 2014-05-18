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

-- TODO: not R5RS compat, doesn't support escaping of internal quotes
-- inside string.
-- TODO: make it so that it can support \n \r \t \\ and so on
parseString :: Parser (LispVal s)
parseString = char '"' *> (String . T.pack <$> many (noneOf "\"")) <* char '"'

-- TODO: update this to work on text
parseAtom :: Parser (LispVal s)
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom (T.pack atom)

-- TODO: should suffice but scheme standard has different base, we can keep
-- things simple and force it to base10 only
-- TODO: may eventually want to support Float for stats
parseNumber :: Parser (LispVal s)
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser (LispVal s)
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser (LispVal s)
parseDottedList = DottedList <$> endBy parseExpr spaces <*> (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser (LispVal s)
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser (LispVal s)
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> char '(' *> (try parseList <|> parseDottedList) <* char ')'

readExpr :: T.Text -> ThrowsError (LispVal s)
readExpr = readOrThrow parseExpr

readExprList :: T.Text -> ThrowsError [LispVal s]
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> T.Text -> ThrowsError a
readOrThrow parser input = case parse parser "Scheme" input of
    Left err  -> throwError $ Parser err
    Right val -> return val
