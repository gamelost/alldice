{-# LANGUAGE OverloadedStrings #-}
module Dice.PCGen
    ( pcgenDicePool
    ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Data.Text as T

import Dice.Common

-- PCGen AST
data PCGenAST = Roll DicePool
              | List [Integer]
              | Number Integer
              | Function T.Text [PCGenAST]
              | Expr T.Text PCGenAST PCGenAST
    deriving Show

-- Helper
digits :: Parser Integer
digits = liftM read $ many1 digit

simpleDicePool :: Parser DicePool
simpleDicePool = DicePool <$> roll <*> dice <*> many modifiers

roll :: Parser Dices
roll = liftM read $ option "1" (many1 digit)

dice :: Parser Dice
dice = char 'd' *> choice
    [ StandardDice <$> digits
    , diceType <$> oneOf "F%"
    ]
  where
    -- TODO: known non-total
    diceType 'F' = FudgeDice
    diceType '%' = StandardDice 100 -- TODO: should this be its own percentage dice type?

modifiers :: Parser Modifiers
modifiers = choice
    [ selectModifier <$> oneOf "/\\tTmM" <*> digits
    , KeepIndex <$> (char '|' *> ((liftM (+ (-1)) digits) `sepBy1` char ',')) -- 1-indexed
    ]
  where
    -- TODO: known non-total
    selectModifier '/'  d = KeepTop d
    selectModifier '\\' d = KeepBottom d
    selectModifier 't'  d = MinTotal d
    selectModifier 'T'  d = MaxTotal d
    selectModifier 'm'  d = RerollBelow d
    selectModifier 'M'  d = RerollAbove d




pcgenDicePool :: Parser PCGenAST
pcgenDicePool = spaces *> choice
    [ try $ between (string "roll(\"") (string "\")") testExpr
    , testExpr
    ] <* spaces <* eof

astStuff :: Parser PCGenAST
astStuff = spaces *> choice
    [ try (Roll <$> simpleDicePool)
    , List <$> parseSequence '[' ']' digits
    , Number <$> digits
    , try (Function <$> liftM T.pack (many1 $ noneOf "(") <*> parseSequence '(' ')' astStuff)
    , between (char '(' <* spaces) (spaces *> char ')') testExpr
    ] <* spaces

parseSequence :: Char -> Char -> Parser a -> Parser [a]
parseSequence open close parser = between (char open <* spaces) (spaces *> char close) ((parser <* spaces) `sepBy` (char ',' <* spaces))


testExpr = buildExpressionParser table astStuff

table = [ [op "+" (Expr "+") AssocLeft]
        , [op "-" (Expr "-") AssocLeft]
        ]
  where
    op s f a = Infix (do { string s; return f }) a

test :: [T.Text]
test =
    [ "1"
    , "[1,2]"
    , "1+2"
    , "1d6+2"
    , "2+1d6"

    , "d6"
    , "2d6"
    , "4d6/3"
    , "4d6\\3"
    , "5d6|1,4"
    , "d6+3"
    , "d6-3"
    , "10d6t10"
    , "10d6T20"
    , "10d6m10"
    , "10d6M20"
    , "20d10/10\\2T20t2m3M5+22"
    , "20d10/10\\2T20t2m3M5|2,3,4+22"
    , "d%"
    , "2d%"
    , "dF"
    , "2dF"

    , "roll(\"1\")"
    , "roll(\"[1,2]\")"

    , "roll(\"roll(1,2)\")"
    , "roll(\"roll(1,2,[3,4])\")"
    , "roll(\"roll(1,[2,3])\")"
    , "roll(\"roll(1,[2,3],[4,5])\")"
    , "roll(\"roll(1,2,3,4,5)\")"

    , "roll(\"max(4,7)\")"
    , "roll(\"min(4,7)\")"
    , "roll(\"pow(10,2)\")"

    , "roll(\"roll(4,6,reroll(1))\")"
    , "roll(\"roll(4,6,top(3))\")"
    , "roll(\"roll(4,6,top(3),reroll(1))\")"

    , "roll(1,2)"
    , "roll(1,2,[3,4])"
    , "roll(1,[2,3])"
    , "roll(1,[2,3],[4,5])"
    , "roll(1,2,3,4,5)"

    , "max(4,7)"
    , "min(4,7)"
    , "pow(10,2)"

    , "roll(4,6,reroll(1))"
    , "roll(4,6,top(3))"
    , "roll(4,6,top(3),reroll(1))"

    , "nop()"
    , "roll(\"nop()\")"

    , "roll(\"1d10\")"
    , "roll(\"1d20+10\")"
    , "roll(\"10+1d10\")"

    , " 1 "
    , " [ 1 , 2 ] "
    , " [1, 2] "
    , " 1 + 2 "
    , " 1d6 + 2 "
    , " 2 + 1d6 "
    , " 1d6 + 2d6 "

    , "roll(1, [2, 3])"

    , " roll(\" 1 \") "
    , " roll( 1 , 2 ) "
    , " roll( 1 , [ 2 , 3 ] ) "
    , " roll( 4 , 6 , reroll( 1 ) ) "
    , "nop( )"

    , "1+(2+3)"
    , " 1 + ( 2 + 3 ) "
    , " 20d10/10\\2T20t2m3M5|2,3,4 + ( 10d20 - 2d6 ) "
    ]
