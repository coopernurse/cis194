module Main where

<<<<<<< HEAD
import Cis194.Hw.Editor
import Cis194.Hw.JoinList
import Cis194.Hw.Scrabble
import Cis194.Hw.Sized
=======
import Editor
import JoinList
import Scrabble
import Sized
>>>>>>> week 7: JoinListBufEditor impl

main = runEditor editor $ (Empty :: JoinList (Score, Size) String)
