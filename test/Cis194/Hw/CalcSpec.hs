module Cis194.Hw.CalcSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Calc
import Cis194.Hw.ExprT

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Calculator - eval" $ do
    it "should add and multiply numbers" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "Calculator - evalStr" $ do
    it "should evaluate well formed strings" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20
    it "evals the multiplication operator before addition" $ do
      evalStr "2+3*4" `shouldBe` Just 14
    it "returns Nothing if the string is malformed" $ do
      evalStr "2+3*" `shouldBe` Nothing

