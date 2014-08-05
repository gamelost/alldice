{-# LANGUAGE OverloadedStrings #-}
module Dice.PCGen
    ( dice
    ) where

import Control.Applicative hiding ((<|>), many)
import Control.Monad
import Text.Parsec hiding (spaces)
import Text.Parsec.Text
import qualified Data.Text as T

import Dice.Common


dice :: Parser DicePool
dice = undefined








test :: [T.Text]
test =
    [ "d6"
    , "2d6"
    , "4d6/3"
    , "4d6\\3"
    , "5d6|1,4"
    , "2d6m2"
    , "2d6M2"
    , "d6+3"
    , "d6-3"
    , "10d6t10"
    , "10d6T20"
    ]
