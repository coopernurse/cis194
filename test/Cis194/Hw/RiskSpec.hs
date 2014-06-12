module Cis194.Hw.RiskSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Cis194.Hw.Risk
import Control.Monad.Random
import Control.Applicative

main :: IO ()
main = hspec spec

newtype SmallBattle = SmallBattle Battlefield
  deriving Show

instance Arbitrary SmallBattle where 
  arbitrary = (\x y -> SmallBattle $ Battlefield (x `mod` 6) (y `mod` 6)) <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "battle" $ do
    it "should always return attacker/defender counts >= 0" $ do
      property $ \(SmallBattle b) -> do
        res <- evalRandIO $ battle b
        (attackers res) `shouldSatisfy` (>=0)
        (defenders res) `shouldSatisfy` (>=0)
