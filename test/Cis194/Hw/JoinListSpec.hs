module Cis194.Hw.JoinListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Hw.JoinList

main :: IO ()
main = hspec spec

indexJProp :: Int -> JoinList 

spec :: Spec
spec = do
  describe "indexJ" $ do
    -- uses QuickCheck - need to learn how to generate constrained inputs
    it "indexJ == jlToList with !!?" $ property $
      \i jl ->  (indexJ i jl) == (jlToList jl !!? i)
