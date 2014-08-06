{-# LANGUAGE OverloadedStrings #-}
module Dice.PCGen
    ( pcgenDicePool
    ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec hiding (spaces)
import Text.Parsec.Text
import qualified Data.Text as T

import Dice.Common

-- Helper
digits :: Parser Integer
digits = liftM read $ many1 digit

pcgenDicePool :: Parser DicePool
pcgenDicePool = choice
    [ simpleDicePool
    , complicatedDicePool
    ]

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
    [ selectModifier <$> oneOf "/\\+-tTmM" <*> digits
    , KeepIndex <$> (char '|' *> (digits `sepBy1` char ','))
    ]
  where
    -- TODO: known non-total
    -- Optional literal 'm' (minimum) followed by positive integer, rerollAbove, or literal 'M' (maximum) followed by postive integer, rerollBelow.
    selectModifier '/'  d = KeepTop d
    selectModifier '\\' d = KeepBottom d
    selectModifier '+'  d = Constant d
    selectModifier '-'  d = Constant (-d)
    selectModifier 't'  d = MinTotal d
    selectModifier 'T'  d = MaxTotal d
    selectModifier 'm'  d = RerollBelow d
    selectModifier 'M'  d = RerollAbove d


complicatedDicePool :: Parser DicePool
complicatedDicePool = DicePool <$> roll <*> dice <*> many modifiers

--    , "roll(4,5,[2,3,4])"
--    , "roll(4,6,reroll(1))"
--    , "roll(4,6,top(3))"
--    , "roll(4,6,top(3),reroll(1))"


--type Dices = Integer
--data Dice = StandardDice Integer -- ^ Dice with [1..N] sides
--          | ListDice [Integer] -- ^ Used for non-standard dices, each integer in the list is a side
--          | FudgeDice -- ^ Used for fudge dices (1-2 -, 3-4 0, 5-6 +) for ex
--data Modifiers = Constant Integer -- ^ Addition or substraction (IE. +6, -2)
--               | KeepTop Integer -- ^ Keep top x dices out of a pool
--               | KeepBottom Integer -- ^ Keep bottom x dices out of a pool
--               | KeepIndex [Integer] -- ^ Keep x dices out of a pool (0-indexed sorted ascending)
--               | MinTotal Integer -- ^ Min of x total
--               | MaxTotal Integer -- ^ Max of x total
--               | RerollBelow Integer -- ^ Reroll if below x
--               | RerollAbove Integer -- ^ Reroll if above x
--data DicePool = DicePool Dices Dice [Modifiers]




test :: [T.Text]
test =
    [ "d6"
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
    , "d%"
    , "2d%"
    , "dF"
    , "2dF"

    , "roll(4,5,[2,3,4])"
    , "roll(4,6,reroll(1))"
    , "roll(4,6,top(3))"
    , "roll(4,6,top(3),reroll(1))"

    , "roll(\"1d10\")"
    , "roll(\"1d20+10\")"
    , "roll(\"roll(1,[3,5,7,9])\")"
    , "roll(\"roll(3,6)\")"
    , "roll(\"roll(3,[2,3,4,5,6],[2,3])\")"
    , "roll(\"roll(4,6,[2,3,4])\")"

--    , "roll(\"10+d10\")"
--    , "roll()"
--    , "roll(\"x\")"
--    , "roll(1,20,|VAR|,'text\n')"
--    , "roll(\"max(4,7)\")"
--    , "roll(\"min(4,7)\")"
--    , "roll(\"pow(10,2)\")"
    ]
