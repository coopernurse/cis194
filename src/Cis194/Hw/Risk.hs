{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw.Risk where

import Control.Monad
import Control.Monad.Random
import Control.Applicative
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

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving (Show)

-- EX 2
-- write a function with the type
--
-- battle :: Battlefield -> Rand StdGen Battlefield
--
-- ...which simulates a single battle (as explained above)
-- between two opposing armies. That is, it should simulate
-- randomly rolling the appropriate number of dice,
-- interpreting the results, and updating the two armies to
-- reflect casualties. You may assume that each player will
-- attack or defend with the maximum number of units they
-- are allowed.

casualties :: [DieValue] -> [DieValue] -> (Int, Int)
casualties atkRolls dfdRolls = foldl (reducer) (0, 0) pairs
  where pairs = zipWith (flip compare) (sort atkRolls) (sort dfdRolls)
        reducer (a, d) result = if result == GT then (a, d+1) else (a+1, d)

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) =
  replicateM (min (a - 1) 3) die >>= \aRolls ->
  replicateM (min d 2) die       >>= \dRolls ->
  let
    c = casualties aRolls dRolls
  in
    return $ Battlefield (a - fst c) (d - snd c)

-- EX 3
-- Now implement a function
-- ￼
-- invade :: Battlefield -> Rand StdGen Battlefield
--
-- which simulates an entire invasion attempt, that is,
-- repeated calls to battle until there are no defenders
-- remaining, or fewer than two attackers.

invade :: Battlefield -> Rand StdGen Battlefield
invade initial = battle initial >>= check
  where
    check result@(Battlefield attackers defenders)
      | defenders == 0 || attackers < 2 = return result
      | otherwise = invade result

-- EX 4
-- Finally, implement a function
--
-- successProb :: Battlefield -> Rand StdGen Double
--
-- which runs invade 1000 times, and uses the results to
-- compute a Double between 0 and 1 representing the
-- estimated probability that the attacking army will
-- completely destroy the defending army.
--
-- For example, if the defending army is destroyed in 300
-- of the 1000 simulations (but the attacking army is
-- reduced to 1 unit in the other 700), successProb should
-- return 0.3.

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  replicateM 1000 (invade b) >>= \results ->
  return $ fromIntegral (length $ filter ((==0) . defenders) results) / 1000.0
