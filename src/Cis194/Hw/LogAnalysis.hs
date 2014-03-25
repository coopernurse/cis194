{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import  Cis194.Hw.Log

-- parse a log entry to a LogMessage
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("W":n:xs) -> LogMessage Warning (read n) (unwords xs)
  ("I":n:xs) -> LogMessage Info (read n) (unwords xs)
  ("E":n1:n2:xs) -> LogMessage (Error (read n1)) (read n2) (unwords xs)
  (_) -> Unknown s

-- assume the string contains a log entry per line
parse :: String -> [LogMessage]
parse [] = []
parse s  = map parseMessage . lines $ s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m@(LogMessage _ ts1 _) t = case t of
  (Leaf) -> Node Leaf m Leaf
  (Node _ (Unknown _) _) -> t
  (Node t1 m2@(LogMessage _ ts2 _) t2) -> case ts1 < ts2 of
    True  -> Node (insert m t1) m2 t2
    False -> Node t1 m2 (insert m t2)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build messages = foldl (\tree message -> insert message tree) Leaf messages

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node t1 m t2) = (inOrder t1) ++ [m] ++ (inOrder t2)

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map (\(LogMessage _ _ m) -> m) $ inOrder $ build $ filter isRelevant messages

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error s) _ _) = s > 49
isSevere _ = False

isRelevant :: LogMessage -> Bool
isRelevant m = isError m && isSevere m
