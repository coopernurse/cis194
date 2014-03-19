module Cis194.Hw.LogAnalysisSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.Log
import Cis194.Hw.LogAnalysis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LogAnalysis" $ do
    it "should parse error lines" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"

    it "should parse info lines" $ do
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"

    it "should parse unknown lines" $ do
      parseMessage "This is not in the right format" `shouldBe`  Unknown "This is not in the right format"

