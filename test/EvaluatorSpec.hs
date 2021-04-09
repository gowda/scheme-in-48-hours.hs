module EvaluatorSpec where

import Evaluator (eval)
import Lib (readExpr)
import Test.Hspec (describe, it, shouldBe)
import Types (LispVal (..))

spec = do
  describe "eval" $ do
    it "evaluates 2 to 2" $ do
      (eval $ Number 2) `shouldBe` (Number 2)

    it "evaluates (+ 2 (- 4 1)) to 5" $ do
      (eval $ readExpr "(+ 2 (- 4 1))") `shouldBe` (Number 5)

    it "evaluates (- (+ 4 6 3) 3 5 2) to 2" $ do
      (eval $ readExpr "(- (+ 4 6 3) 3 5 2)") `shouldBe` (Number 3)
