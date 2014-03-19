{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Cis194.Hw.Log

-- TODO: need to understand error handling. this implementation will blow up
-- if the error severity or timestamp is not an integer (read throws an exception)
-- sample.log and error.log are well formed, so this isn't an issue for parsing those files..
--
-- It's also unclear what the expected output is for messages without timestamps
-- e.g.:   I Foo
--
parseMessage :: String -> LogMessage
parseMessage s = case mtype of
    "I" -> LogMessage Info time msg
    "W" -> LogMessage Warning time msg
    "E" -> LogMessage (Error sev) time msg
    _   -> Unknown s
  where xs          = words s
        (mtype,xs1) = if length xs == 0 then ("",xs) else (head xs, tail xs)
        (sev,xs2)   = if mtype == "E" && length xs1 > 1 then (read (head xs1), (tail xs1)) else (-1, xs1)
        (time,xs3)  = if length xs2 > 1 then (read (head xs2), (tail xs2)) else (-1, xs2)
        msg         = unwords xs3

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t                = t
insert m@(LogMessage _ newtime _) t = case t of
  Leaf              -> Node Leaf m Leaf
  Node left n right -> case n of
    (Unknown _)           -> Node Leaf m Leaf
    (LogMessage _ time _) -> if newtime < time 
      then Node (insert m left) n right 
      else Node left n (insert m right)

build :: [LogMessage] -> MessageTree
build msgs = foldl (\tree msg -> insert msg tree) Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                  = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

filterErrs :: LogMessage -> Bool
filterErrs (LogMessage (Error sev) _ _) = sev >= 50
filterErrs _ = False

filterBeforeFailure :: LogMessage -> Bool
filterBeforeFailure (LogMessage _ time _) = time > -1 && time <= 5395
filterBeforeFailure _ = False

toMsg :: LogMessage -> String
toMsg (LogMessage _ _ msg) = msg
toMsg _ = ""

toMsgFull :: LogMessage -> String
toMsgFull (LogMessage mtype time msg) = (show mtype) ++ " " ++ (show time) ++ " " ++ msg
toMsgFull _ = ""

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong unsorted = map toMsg errors
  where sorted = inOrder (build unsorted)
        errors = filter filterErrs sorted

beforeFailure :: [LogMessage] -> [String]
beforeFailure unsorted = map toMsgFull errors
  where sorted = inOrder (build unsorted)
        errors = filter filterBeforeFailure sorted

allSorted :: [LogMessage] -> [String]
allSorted unsorted = map toMsg sorted
  where sorted = inOrder (build unsorted)

