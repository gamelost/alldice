{-# LANGUAGE OverloadedStrings #-}
module Dice.Common
    ( Dices(..)
    , Dice(..)
    , Modifiers(..)
    , DicePool(..)
    ) where

-- | Number of dices in a dice pool
type Dices = Integer

-- | The dice itself
data Dice = StandardDice Integer -- ^ Dice with [1..N] sides
          | ListDice [Integer] -- ^ Used for non-standard dices, each integer in the list is a side
          | FudgeDice -- ^ Used for fudge dices (1-2 -, 3-4 0, 5-6 +) for ex
    deriving Show

-- | Modifiers that affects a dice pool
data Modifiers = Constant Integer -- ^ Addition or substraction (IE. +6, -2)
               | KeepTop Integer -- ^ Keep top x dices out of a pool
               | KeepBottom Integer -- ^ Keep bottom x dices out of a pool
               | KeepIndex [Integer] -- ^ Keep x dices out of a pool (0-indexed sorted ascending)
               | MinTotal Integer -- ^ Min of x total
               | MaxTotal Integer -- ^ Max of x total
               | RerollBelow Integer -- ^ Reroll if below x
               | RerollAbove Integer -- ^ Reroll if above x
    deriving Show

-- | The Dice Pool
data DicePool = DicePool Dices Dice [Modifiers]
    deriving Show
