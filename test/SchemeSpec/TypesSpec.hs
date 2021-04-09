module SchemeSpec.TypesSpec where

import Scheme.Types (LispVal (..))
import Test.Hspec (describe, it, shouldBe)

spec = do
  describe "show" $ do
    it "returns right booleans" $ do
      show (Bool True) `shouldBe` "#t"
