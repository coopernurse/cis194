module Cis194.Hw.JoinListBufEditor where

import Cis194.Hw.Editor
import Cis194.Hw.JoinList
import Cis194.Hw.Scrabble
import Cis194.Hw.Sized

run = runEditor editor $ (Empty :: JoinList (Score, Size) String)
