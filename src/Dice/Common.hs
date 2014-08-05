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

-- | Modifiers that affects a dice pool
data Modifiers = Constant Integer -- ^ Addition or substraction (IE. +6, -2)
               | KeepTop Integer -- ^ Keep top x dices out of a pool
               | KeepBottom Integer -- ^ Keep bottom x dices out of a pool
               | KeepIndex [Integer] -- ^ Keep x dices out of a pool (0-indexed sorted ascending)
               | MinTotal Integer -- ^ Min of x total
               | MaxTotal Integer -- ^ Max of x total

-- | The Dice Pool
data DicePool = DicePool Dices Dice [Modifiers]
