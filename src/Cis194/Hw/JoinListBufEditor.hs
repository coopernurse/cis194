module Main where

import Editor
import JoinList
import Scrabble
import Sized

main = runEditor editor $ (Empty :: JoinList (Score, Size) String)
