{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Monoid
import Data.Ratio

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving Show

----------
-- Ex. 1
----------

{- MonadRandom installed and working.

Examples:
> getRandom
> getRandomR (1,6)
> uniform [1..6]
> evalRandIO die
> getRandoms >>= return . take 10
> getRandomRs (1,6) >>= return . take 10
To check that sides are not loaded (frequencies are similar):
> getRandomRs (1,6) >>= return . map length . group . sort . take 10000

-}

----------
-- Ex. 2
----------

type Offensives = Army
type Defensives = Army

-- Determine how many attacking units can be deployed
offensive :: Battlefield -> Offensives
offensive bf | att <= 1 = 0
             | otherwise = min 3 $ pred att
  where att = attackers bf

-- Determine how many defending units can be mobilised
defensive :: Battlefield -> Defensives
defensive bf | def <= 0 = 0
             | otherwise = min 2 def
  where def = defenders bf

-- Roll a few dice
dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

-- Determine odds as die values for offensive or defensive manoeuvre
battleOdds :: (Battlefield -> Army) -> Battlefield -> Rand StdGen [DieValue]
battleOdds manoeuvre = liftM sortRolls . dice . manoeuvre

sortRolls :: [DieValue] -> [DieValue]
sortRolls = reverse . sort

type Attackers = Sum Int
type Defenders = Sum Int
type Deaths = (Attackers, Defenders)

-- The rules of death
outcome :: Ord a => a -> a -> Deaths
outcome = \roll1 roll2 ->
           if roll1 > roll2
           then (Sum 0, Sum 1) -- one defender unit dies
           else (Sum 1, Sum 0) -- one attacker unit dies

-- Find out who dies after rolling each pair of dice
unitKills :: Battlefield -> Rand StdGen [Deaths]
unitKills bf = liftM2 (zipWith outcome)
                      (battleOdds offensive bf)
                      (battleOdds defensive bf)

-- Sum up all deaths
totalKills :: [Deaths] -> Deaths
totalKills = foldr (<>) mempty

-- Scythe those who must die from the battlefield
kill :: Deaths -> Battlefield -> Battlefield
kill (Sum attackerDeaths, Sum defenderDeaths) (Battlefield a d)
  = Battlefield (a - attackerDeaths) (d - defenderDeaths)

-- Commence the battle and kill some units
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = liftM2 kill deathMobit (return bf) where
  deathMobit = liftM totalKills $ unitKills bf

-- Example battlefield
battlefield :: Battlefield
battlefield = Battlefield 5 2

bf2 :: Battlefield
bf2 = Battlefield 3 1

----------
-- Ex. 3
----------
{-
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  aftermath <- battle bf
  if attackers aftermath < 2 || defenders aftermath == 0
    then return aftermath
    else invade aftermath
-}
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = 
  if attackers bf < 2 || defenders bf == 0
    then return bf
    else do aftermath <- battle bf
            invade aftermath

----------
-- Ex. 4
----------

successProb :: Battlefield -> Rand StdGen Double
successProb bf = liftM2 divide successes (return trials) where
  successes = fmap (length . filter defendersGone) invasions
  trials = 30000
  invasions = replicateM trials $ invade bf
  defendersGone = (== 0) . defenders
  divide = \n d -> fromIntegral n / fromIntegral d

data Card = Spade | Heart | Diamond | Club

----------
-- Ex. 5
----------

-- Incorrect
battleProb :: Army -> Army -> Rational
battleProb a d
  | d < 1 = 1
  | d > 1 = battleProb a (d - 1) * battleProb (a - 1) (d - 1)
  | otherwise = 1 - summa % sampleSpace where
      summa = sum (map (^a) [1..6])
      sampleSpace = 6^(a + d)

-- Incorrect
invasionProb :: Army -> Army -> Rational
invasionProb a d = battleProb a d +
                 if a <= 1
                 then 0
                 else (1 - battleProb a d) * invasionProb (a - 1) d

-- Mostly correct, gives slightly wrong probabilities with a >= 3 and d == 2
-- and grossly wrong probabilities with d >= 3
invasionProb2 :: Army -> Army -> Rational
invasionProb2 a d
  | a <= 0 && d > 0 = 0
  | d <= 0 = 1
  | otherwise = (rollOdds2 a d) * (invasionProb2 a (d - 1)) +
                (1 - rollOdds2 a d) * invasionProb2 (a - 1) d

-- Produce all possible variations after rolling n dice 
diceComb :: Int -> [[Int]]
diceComb n
  | n == 0 = [[]]
  | otherwise = [(x:xs) | x  <- [1..6], xs <- diceComb (pred n)]

-- Seems correct
rollOdds2 :: Int -> Int -> Rational
rollOdds2 a d | a <= 0 && d > 0 = 0
rollOdds2 a d
  | d <= 0 = 1
  | otherwise = fromIntegral greater % fromIntegral total where
      greater = length $ filter biggerHead dc
      biggerHead xs = maximum (take a xs) > maximum (drop a xs)
      total = length dc
      dc = diceComb (a + d)

-- For now, works only with battlefields up to Battlefield 4 2 (five dice)
exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf = fromRational $
                      invasionProb2 (offensive bf) (defensive bf)

-- Midagi muud
sixtimes :: Num a => a -> a
sixtimes = (*2) . (*3)

