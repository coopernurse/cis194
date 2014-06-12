{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw.Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show, Eq)

-----------------------------
-- Ex 2. 
-----------------------------

-- simulates a single battle between two opposing armies. That is, 
-- it should simulate randomly rolling the appropriate number of dice, 
-- interpreting the results, and updating the two armies to reﬂect 
-- casualties. You may assume that each player will attack or defend 
-- with the maximum number of units they are allowed
--
battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  attRolls <- sequence $ replicate (max 3 att) die             -- roll die max of 3 times
  defRolls <- sequence $ replicate (max 2 def) die             -- roll die max of 2 times
  let attWin = zipWith (>) (sortR attRolls) (sortR defRolls)   -- creates [Bool] where True=attacker won
  return $ Battlefield (survivors att False attWin) (survivors def True attWin)
  where
    att = attackers b 
    def = defenders b
    sortR rs = reverse . sort $ map unDV rs            -- turn rolls into [Int] and sort in descending order
    survivors orig lose attWin = 
      max 0 (orig - (length $ filter (==lose) attWin)) -- subtract # losses from orig count

-- notes:
-- to print 3 rolls in repl:
-- mapM evalRandIO $ replicate 3 die

-----------------------------
-- Ex 3. 
-----------------------------

-- simulates an entire invasion attempt, that is, repeated calls
-- to battle until there are no defenders remaining, or fewer than two
-- attackers.
--
invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  result <- battle b
  if (attackers result) < 2 || (defenders result) < 1
    then return result   -- either attackers or defenders are depleted. invasion over.
    else invade result   -- fight another round

-----------------------------
-- Ex 4. 
-----------------------------

-- runs invade 1000 times, and uses the results to compute a
-- Double between 0 and 1 representing the estimated probability that
-- the attacking army will completely destroy the defending army.
-- 
-- For example, if the defending army is destroyed in 300 of the 1000
-- simulations (but the attacking army is reduced to 1 unit in the other
-- 700), successProb should return 0.3.
--
successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  results <- replicateM 1000 $ invade b                          -- invade 1000 times
  let wins = length $ filter (\r -> (defenders r) == 0) results  -- get # of results where attackers won
  return ((fromIntegral wins) / 1000.0)

-----------------------------
-- Ex 5. (Optional)
-----------------------------

-- computes the exact probability of success based on principles
-- of probability, without running any simulations. (This won’t give you
-- any particular practice with Haskell; it’s just a potentially interesting
-- challenge in probability theory.)
--
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined

