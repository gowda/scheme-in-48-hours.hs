module TypesSpec where

import Test.Hspec (describe, it, shouldBe)
import Types (LispVal (..))

spec = do
  describe "show" $ do
    it "returns right booleans" $ do
      show (Bool True) `shouldBe` "#t"
