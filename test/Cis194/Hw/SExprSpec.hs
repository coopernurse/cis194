module Cis194.Hw.SExprSpec (main, spec) where

import Test.Hspec
import Cis194.Hw.SExpr
import Cis194.Hw.AParser
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "zeroOrMore" $ do
    it "returns an empty list if no matches" $ do
      runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Just ("","abcdeFGh")

    it "returns the substring, if a match is found" $ do
      runParser (zeroOrMore (satisfy isUpper)) "ABCdeFGh" `shouldBe` Just ("ABC","deFGh")

  describe "oneOrMore" $ do
    it "returns Nothing if no match" $ do
      runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" `shouldBe` Nothing

    it "returns the substring, if a match is found" $ do
      runParser (oneOrMore (satisfy isUpper)) "ABCdeFGh" `shouldBe` Just ("ABC","deFGh")

  describe "spaces" $ do
    it "parses a consecutive list of one or more whitespace characters" $ do
      runParser (spaces) "Hello World" `shouldBe` Just ("", "Hello World")
      runParser (spaces) "   Hello World" `shouldBe` Just ("   ", "Hello World")

  describe "ident" $ do
    -- Next, ident should parse an identifier, which for our
    -- purposes will be an alphabetic character (use isAlpha)
    -- followed by zero or more alphanumeric characters (use
    -- isAlphaNum).
    it "parses an identifier: an alpha char followed by zero or more alphanums" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar"," baz")
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA","")
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing
