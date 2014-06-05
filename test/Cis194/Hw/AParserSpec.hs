module Cis194.Hw.AParserSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.AParser
import Control.Applicative

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "char" $ do
    it "looks for the supplied Char in the string" $ do
      runParser (char 'x') "xyz" `shouldBe` Just ('x', "yz")
      runParser (char 'x') "abxyz" `shouldBe` Nothing

  describe "abParser" $ do
    it "looks for the sequence of chars: 'a' and 'b' and returns them as a pair" $ do
      runParser abParser "xyz" `shouldBe` Nothing
      runParser abParser "abcd" `shouldBe` Just (('a', 'b'), "cd")

  describe "abParser_" $ do
    it "returns () instead of ('a', 'b') but otherwise does what abParser does" $ do
      runParser abParser_ "xyz" `shouldBe` Nothing
      runParser abParser_ "abcd" `shouldBe` Just ((), "cd")

  describe "intPair" $ do
    it "returns the two parsed integers in a list" $ do
      runParser intPair "abcd" `shouldBe` Nothing
      runParser intPair "12asd" `shouldBe` Nothing
      runParser intPair " 1 2asd" `shouldBe` Nothing
      runParser intPair "1 2asdasd" `shouldBe` Just ([1, 2], "asdasd")

  describe "fmap on Parser" $ do
    it "applies the function f to the output of a given Parser" $ do
      runParser (fmap (+3) posInt) "123asd" `shouldBe` Just (126, "asd")
      runParser (fmap (+3) posInt) "asd" `shouldBe` Nothing

  describe "applicative on Parser" $ do
    it "combines parsers" $ do
      let combiner = (\x y -> x:y:[])
      let p1 = combiner <$> char 'a' <*> char 'b'
      let p2 = combiner <$> char 'x' <*> char 'y'

      runParser p1 "abcdefg" `shouldBe` Just ("ab", "cdefg")
      runParser p2 "abcdefg" `shouldBe` Nothing

  describe "Parser instance of Alternative" $ do
    it "chooses the output of the parser that passes" $ do
      runParser (char 'a' <|> char 'b') "bcd" `shouldBe` Just ('b', "cd")
      runParser (char 'a' <|> char 'b') "qcd" `shouldBe` Nothing

  describe "intOrUppercase" $ do
    it "parses either an integer value or an uppercase character, and fails otherwise" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "foo" `shouldBe` Nothing
