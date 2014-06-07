{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw.Risk where

import Control.Monad.Random

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
battle = undefined

-----------------------------
-- Ex 3. 
-----------------------------

-- simulates an entire invasion attempt, that is, repeated calls
-- to battle until there are no defenders remaining, or fewer than two
-- attackers.
--
invade :: Battlefield -> Rand StdGen Battlefield
invade = undefined

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
successProb = undefined

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

