{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Applicative
import Control.Monad
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

-- Roll the die n times, sort the results (descending)
roll :: Int -> Rand StdGen [DieValue]
roll n = sortBy (flip compare) <$> replicateM n die

losses ::  (DieValue,DieValue) -> Battlefield -> Battlefield
losses (aroll,droll) (Battlefield a d)
  | aroll > droll = Battlefield a (d-1)
  | otherwise     = Battlefield (a-1) d

battle :: Battlefield -> Rand StdGen Battlefield
battle start = 
  roll (min 3 (attackers start - 1)) >>= \attackRoll ->
  roll (min 2 (defenders start))     >>= \defenseRoll ->
  return $ foldr losses start (zip attackRoll defenseRoll)
