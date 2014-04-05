module Cis194.Hw.AcceptHanoi where

import Cis194.Hw.Week1

data HanoiState = HanoiState3 [Int] [Int] [Int]
                | HanoiState4 [Int] [Int] [Int] [Int]
  deriving (Show, Eq)

-- Add disc to appropriate peg only if peg is empty or disc is smaller than top disc on peg.
placeHanoi :: HanoiState -> String -> Int -> Maybe HanoiState
placeHanoi (HanoiState3 [] b c) ['a'] disc = Just (HanoiState3 [disc] b c)
placeHanoi (HanoiState3 a [] c) ['b'] disc = Just (HanoiState3 a [disc] c)
placeHanoi (HanoiState3 a b []) ['c'] disc = Just (HanoiState3 a b [disc])
placeHanoi (HanoiState4 [] b c d) ['a'] disc = Just (HanoiState4 [disc] b c d)
placeHanoi (HanoiState4 a [] c d) ['b'] disc = Just (HanoiState4 a [disc] c d)
placeHanoi (HanoiState4 a b [] d) ['c'] disc = Just (HanoiState4 a b [disc] d)
placeHanoi (HanoiState4 a b c []) ['d'] disc = Just (HanoiState4 a b c [disc])
placeHanoi (HanoiState3 (a:as) b c) ['a'] disc
  | disc > a = Nothing
  | otherwise = Just (HanoiState3 (disc:a:as) b c)
placeHanoi (HanoiState3 a (b:bs) c) ['b'] disc
  | disc > b = Nothing
  | otherwise = Just (HanoiState3 a (disc:b:bs) c)
placeHanoi (HanoiState3 a b (c:cs)) ['c'] disc
  | disc > c = Nothing
  | otherwise = Just (HanoiState3 a b (disc:c:cs))
placeHanoi (HanoiState4 (a:as) b c d) ['a'] disc
  | disc > a = Nothing
  | otherwise = Just (HanoiState4 (disc:a:as) b c d)
placeHanoi (HanoiState4 a (b:bs) c d) ['b'] disc
  | disc > b = Nothing
  | otherwise = Just (HanoiState4 a (disc:b:bs) c d)
placeHanoi (HanoiState4 a b (c:cs) d) ['c'] disc
  | disc > c = Nothing
  | otherwise = Just (HanoiState4 a b (disc:c:cs) d)
placeHanoi (HanoiState4 a b c (d:ds)) ['d'] disc
  | disc > d = Nothing
  | otherwise = Just (HanoiState4 a b c (disc:d:ds))
placeHanoi _ _ _ = Nothing

-- Move disc from one peg to another if move is legal.
moveHanoi :: HanoiState -> Move -> Maybe HanoiState
moveHanoi (HanoiState3 [] _ _) (['a'],_) = Nothing
moveHanoi (HanoiState3 _ [] _) (['b'],_) = Nothing
moveHanoi (HanoiState3 _ _ []) (['c'],_) = Nothing
moveHanoi (HanoiState4 [] _ _ _) (['a'],_) = Nothing
moveHanoi (HanoiState4 _ [] _ _) (['b'],_) = Nothing
moveHanoi (HanoiState4 _ _ [] _) (['c'],_) = Nothing
moveHanoi (HanoiState4 _ _ _ []) (['d'],_) = Nothing
moveHanoi (HanoiState3 (a:as) b c) (['a'],to) = placeHanoi (HanoiState3 as b c) to a
moveHanoi (HanoiState3 a (b:bs) c) (['b'],to) = placeHanoi (HanoiState3 a bs c) to b
moveHanoi (HanoiState3 a b (c:cs)) (['c'],to) = placeHanoi (HanoiState3 a b cs) to c
moveHanoi (HanoiState4 (a:as) b c d) (['a'],to) = placeHanoi (HanoiState4 as b c d) to a
moveHanoi (HanoiState4 a (b:bs) c d) (['b'],to) = placeHanoi (HanoiState4 a bs c d) to b
moveHanoi (HanoiState4 a b (c:cs) d) (['c'],to) = placeHanoi (HanoiState4 a b cs d) to c
moveHanoi (HanoiState4 a b c (d:ds)) (['d'],to) = placeHanoi (HanoiState4 a b c ds) to d
moveHanoi _ _ = Nothing

-- use a move to go from a possible state to a new possible state
applyMove :: Maybe HanoiState -> Move -> Maybe HanoiState
applyMove state move = state >>= (`moveHanoi` move)

-- transition from a given state to a new state given a list of moves
evolveHanoi :: HanoiState -> [Move] -> Maybe HanoiState
evolveHanoi initialState moves = foldl applyMove (Just initialState) moves

-- accept only if implementation provides only legal moves that transport all discs from peg a to peg b.
acceptHanoi3 :: (Integer -> Peg -> Peg -> Peg -> [Move]) -> Integer -> Maybe HanoiState
acceptHanoi3 algorithm n = case (evolveHanoi (HanoiState3 [1.. (fromIntegral n)] [] []) (algorithm n "a" "b" "c")) of
  Just (HanoiState3 [] b []) -> Just (HanoiState3 [] b [])
  _ -> Nothing

acceptHanoi4 :: (Integer -> Peg -> Peg -> Peg -> Peg -> [Move]) -> Integer -> Maybe HanoiState
acceptHanoi4 algorithm n = case (evolveHanoi (HanoiState4 [1..(fromIntegral n)] [] [] []) (algorithm n "a" "b" "c" "d")) of
  Just (HanoiState4 [] b [] []) -> Just (HanoiState4 [] b [] [])
  _ -> Nothing


