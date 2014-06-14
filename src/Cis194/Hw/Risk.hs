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

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield as ds) =
  replicateM (min (as - 1) 3) die >>= \ars ->
    replicateM (min ds 2) die >>= \drs ->
      let cs = casualties ars drs
      in return $ Battlefield (as - fst cs) (ds - snd cs)
      where
        casualties ars drs     = foldl (reducer) (0, 0) $ zipWith (<) (sort ars) (sort drs)
        reducer (acs, dcs) cmp = if cmp then (acs, dcs + 1) else (acs + 1, dcs)

-- EX 3

invade :: Battlefield -> Rand StdGen Battlefield
invade initial = battle initial >>= check
  where
    check r@(Battlefield as ds)
      | ds == 0 || as < 2 = return r
      | otherwise         = invade r

-- EX 4

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  replicateM 1000 (invade b) >>= \rs ->
    return $ fromIntegral (length $ filter ((==0) . defenders) rs) / 1000.0
