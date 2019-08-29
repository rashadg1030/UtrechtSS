{-# LANGUAGE InstanceSigs #-}

module MTA.Gambling where

import System.Random
import Data.Tuple.Strict
import Control.Monad.IO.Class
import Control.Monad
import Data.Ratio
import MTA.DecisionTree


data Coin    = H | T
  deriving (Eq, Enum, Bounded, Show)
data Dice    = D1 | D2 | D3 | D4 | D5 | D6
  deriving (Eq, Enum, Bounded, Show)
data Outcome = Win | Lose
  deriving (Eq, Show)

instance Random Coin where
  randomR (lo, hi) g = let (a, g') = randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')

  random :: RandomGen g => g -> (Coin, g)
  random = randomR (minBound, maxBound)

instance Random Dice where
  randomR (lo, hi) g = let (a, g') = randomR (fromEnum lo, fromEnum hi) g in (toEnum a, g')

  random :: RandomGen g => g -> (Dice, g)
  random = randomR (minBound, maxBound)

class Monad m => MonadGamble m where
    toss :: m Coin
    roll :: m Dice

game :: MonadGamble m => m Outcome
game = do
    coins <- replicateM 6 toss
    dice  <- roll
    let heads = length $ filter (==H) coins
    if ((+1) $ fromEnum dice) >= heads then
        return Win
    else
        return Lose

instance MonadGamble IO where
    toss :: IO Coin
    toss = randomIO

    roll :: IO Dice
    roll = randomIO

instance MonadGamble DecisionTree where
    toss :: DecisionTree Coin
    toss = Decision [Result H, Result T]

    roll :: DecisionTree Dice
    roll = Decision [Result D1, Result D2, Result D3, Result D4, Result D5, Result D6]

probabilityOfWinning :: DecisionTree Outcome -> Rational
probabilityOfWinning dTree = countWins dTree % (toInteger $ length dTree)
  where
    countWins :: DecisionTree Outcome -> Integer
    countWins dTree = sum $ fmap score dTree

    score :: Outcome -> Integer
    score Win  = 1
    score Lose = 0

simulate :: IO Outcome -> Integer -> IO Rational
simulate theGame times =
  if times > 0 then
    do
      outcomes <- replicateM (fromInteger times) game
      let numOutcomes = toInteger $ length outcomes
      let numWins = toInteger . length $ filter isWin outcomes
      return $ numWins % numOutcomes
  else
    return 0

isWin :: Outcome -> Bool
isWin Win = True
isWin Lose = False
