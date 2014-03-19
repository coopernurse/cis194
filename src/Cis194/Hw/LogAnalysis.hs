{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Log

parseMessage :: String -> LogMessage
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse _ = []

insert :: LogMessage -> MessageTree -> MessageTree
insert _ t = t

build :: [LogMessage] -> MessageTree
build _ = Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder _ = []

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = []
