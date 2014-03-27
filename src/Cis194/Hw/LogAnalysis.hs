{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw.LogAnalysis where

-- in ghci, you may need to specify an additional include path:
-- Prelude> :set -isrc/Cis194/Hw
import Text.Regex.Posix
import  Cis194.Hw.Log

tokenize :: String -> [[String]]
tokenize = (=~ "^(I|W|E[ \t]+([0-9]+))[ \t]+([0-9]+)[ \t]+(.*)")

parseMessage :: String -> LogMessage
parseMessage s = case (tokenize s) of
  [_:(typ:_):err:time:context:_] -> case (typ) of
    'I' -> LogMessage Info (read time) context
    'W' -> LogMessage Warning (read time) context
    'E' -> LogMessage (Error (read err)) (read time) context
    _ -> Unknown s
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert message Leaf = Node Leaf message Leaf
insert m@(LogMessage _ t _) (Node left o@(LogMessage _ ot _) right)
  | t > ot = Node left o (insert m right)
  | otherwise = Node (insert m left) o right
insert _ t = t

build :: [LogMessage] -> MessageTree
build = (foldr insert Leaf) . reverse

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = (inOrder left) ++ (message : (inOrder right))

logString :: LogMessage -> String
logString (LogMessage _ _ s) = s
logString (Unknown s) = s

-- whatWentWrong takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map logString) . inOrder . build . (filter severe)
  where severe (LogMessage (Error severity) _ _) =  severity >= 50
        severe _ = False
