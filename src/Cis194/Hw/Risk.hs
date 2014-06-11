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

-- rule 1:
-- The attacking player may attack with up to three units
-- at a time. However, they must always leave at least one
-- unit behind.

atk :: Army -> (Army, Army)
atk a
  | a == 0    = (0, 0)
  | a == 1    = (0, 1)
  | a == 2    = (1, 1)
  | a == 3    = (2, 1)
  | otherwise = (participated, chilled)
  where participated = min (3) a
        chilled      = a - participated

-- rule 2:
-- The defending player may defend with up to two units (or
-- only one if that is all they have).

dfd :: Army -> (Army, Army)
dfd a = (participated, chilled)
  where participated = min 2 a
        chilled      = a - participated

-- rule 3:
-- To determine the outcome of a single battle, the
-- attacking and defending players each roll one six-sided
-- die for every unit they have attacking or defending. So
-- the attacking player rolls one, two, or three dice, and
-- the defending player rolls one or two dice.

-- rule 4:
-- The attacking player sorts their dice rolls in
-- descending order. The defending player does the same.

pairRolls :: [DieValue] -> [DieValue] -> [(Int, Int)]
pairRolls atkRolls defRolls = zipWith (\aRoll dRoll -> if aRoll > dRoll then (0, 1) else (1, 0)) (sort atkRolls) (sort defRolls)

casualties :: [(Int, Int)] -> (Int, Int)
casualties pairs = foldl1 (\(a, b) (c, d) -> (a + c, b + d)) pairs

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield as ds) =
  replicateM aParticipated die >>= \atkRolls ->
  replicateM dParticipated die >>= \dfdRolls ->
  let
    (aCasualties, dCasualties) = casualties $ pairRolls atkRolls dfdRolls
  in
    return $ Battlefield (aChilled + aParticipated - aCasualties) (dChilled + dParticipated - dCasualties)
  where
    (aParticipated, aChilled) = atk as
    (dParticipated, dChilled) = dfd ds

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
