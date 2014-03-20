module Cis194.Hw.Week1 where

import qualified Data.Map.Strict as Map

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x | x < 0 = []
toDigits x = toDigitsRecur x []

toDigitsRecur :: Integer -> [Integer] -> [Integer]
toDigitsRecur 0 xs = xs
toDigitsRecur x xs = toDigitsRecur (x `div` 10) (x `mod` 10 : xs)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = foldr doubleIfListLenOdd [] $ xs

doubleIfListLenOdd :: Integer -> [Integer] -> [Integer]
doubleIfListLenOdd x xs | even $ length xs = x : xs
doubleIfListLenOdd x xs = (x*2) : xs

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' (x:xs) = x' : doubleEveryOther' xs
  where x' = if even $ length xs then x
             else x * 2

sumDigits :: [Integer] -> Integer
sumDigits xs = foldr (\x acc -> acc + (sum $ toDigits x)) 0 xs

validate :: Integer -> Bool
validate x = (sumDigits $ (doubleEveryOther $ toDigits x)) `mod` 10 == 0

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discs p1 p2 p3 = hanoiRecur discs p1 p2 p3 []

hanoiRecur :: Integer -> Peg -> Peg -> Peg -> [Move] -> [Move]
hanoiRecur discs p1 p2 p3 moves = case discs of
  0 -> moves
  1 -> (p1, p2) : moves
  _ -> (hanoi (discs-1) p1 p3 p2) ++ [(p1,p2)] ++ (hanoi (discs-1) p3 p2 p1)

leastMoves :: [Move] -> [[Move]] -> [Move]
leastMoves  shortest []    = shortest
leastMoves [] (x:xs)       = leastMoves x xs
leastMoves shortest (x:xs) = if length x < length shortest then leastMoves x xs else leastMoves shortest xs

-- I couldn't figure this one out.. had to adapt from:
-- http://stackoverflow.com/questions/3607161/towers-of-hanoi-with-k-pegs
hanoiFrameStewart :: Integer -> [Peg] -> [Move]
hanoiFrameStewart _ []                = []
hanoiFrameStewart 0 _                 = []
hanoiFrameStewart _ (p1:(p2:[]))      = [(p1,p2)]
hanoiFrameStewart n (p1:(p2:(p3:xs))) = 
  hanoiFrameStewart k ([p1,p3,p2] ++ xs) ++
  hanoiFrameStewart (n-k) ([p1,p2] ++ xs) ++
  hanoiFrameStewart k ([p3,p2,p1] ++ xs)
  where k = if (length xs) == 0 then n - 1 else n `div` 2 

---------------------
-- Hanoi Validator --
---------------------

type HanoiState = Map.Map Peg [Integer]

initHanoiState :: Integer -> [Peg] -> HanoiState
initHanoiState discs pegs = case length pegs of
  0 -> Map.empty
  1 -> Map.fromList [((head pegs), [1..discs])]
  _ -> Map.fromList $ [((head pegs), [1..discs])] ++ map (\peg -> (peg, [])) (tail pegs)

hanoiStateVal :: Peg -> HanoiState -> ([Integer], Bool)
hanoiStateVal peg state = case Map.lookup peg state of
  Nothing -> ([], False)
  Just xs -> (xs, True)

hanoiApplyMove :: Move -> HanoiState -> (HanoiState, Bool)
hanoiApplyMove move state 
  | (not fromOk) || (not toOk) = (state, False)    -- one of the pegs in the move doesn't exist
  | length fromPeg  == 0       = (state, False)    -- there's no disc on the from peg
  | length newToPeg == 1       = (newState, True)  -- to peg was empty, so moving any disc is ok
  | fromVal < (head toPeg)     = (newState, True)  -- disc is smaller than top disc on to peg, which is ok
  | otherwise                  = (state, False)    -- everything else is invalid
  where (from, to)         = move 
        (fromPeg, fromOk)  = hanoiStateVal from state
        (toPeg, toOk)      = hanoiStateVal to   state
        fromVal            = if length fromPeg > 0 then head fromPeg else 0
        newToPeg           = fromVal : toPeg
        newState           = Map.insert to newToPeg (Map.insert from (tail fromPeg) state)

hanoiApplyMoves :: [Move] -> HanoiState -> (HanoiState, Bool)
hanoiApplyMoves [] state = (state, True)
hanoiApplyMoves (x:xs) state = case hanoiApplyMove x state of
  (_, False) -> (state, False)
  (newState, True) -> hanoiApplyMoves xs newState

hanoiMovesValid :: Integer -> [Peg] -> [Move] -> Bool
hanoiMovesValid discs pegs moves = 
  let initial = initHanoiState discs pegs
  in case hanoiApplyMoves moves initial of
    (_, False) -> False
    -- check if result's 2nd peg matches the initial state of the 1st peg
    (result, True) -> (hanoiStateVal (head pegs) initial) == (hanoiStateVal (pegs !! 1) result)



