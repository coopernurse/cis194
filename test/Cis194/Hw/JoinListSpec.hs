module Cis194.Hw.JoinListSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Hw.JoinList
import Cis194.Hw.Sized

main :: IO ()
main = hspec spec

indexJProp :: Int -> JoinList Size String -> Bool
indexJProp i jl = (indexJ i jl) == (jlToList jl !!? i)

spec :: Spec
spec = do
  describe "indexJ" $ do
    -- uses QuickCheck - need to learn how to generate constrained inputs
    --it "indexJ == jlToList with !!?" $ property $ indexJProp
    it "foo" $ 1 == 1
