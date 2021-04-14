module SchemeSpec.EvaluatorSpec where

import Scheme (readExpr)
import Scheme.Evaluator (eval)
import Scheme.Types (LispVal (..))
import Test.Hspec (describe, it, shouldBe)

spec = do
  describe "eval" $ do
    it "evaluates 2 to 2" $ do
      (eval $ Number 2) `shouldBe` Right (Number 2)

    it "evaluates (+ 2 (- 4 1)) to 5" $ do
      (readExpr "(+ 2 (- 4 1))" >>= eval) `shouldBe` Right (Number 5)

    it "evaluates (- (+ 4 6 3) 3 5 2) to 2" $ do
      (readExpr "(- (+ 4 6 3) 3 5 2)" >>= eval) `shouldBe` Right (Number 3)

    it "evaluates (< 2 3) to #t" $ do
      (readExpr "(< 2 3)" >>= eval) `shouldBe` Right (Bool True)

    it "evaluates (> 2 3) to #f" $ do
      (readExpr "(> 2 3)" >>= eval) `shouldBe` Right (Bool False)

    it "evaluates (>= 3 3) to #t" $ do
      (readExpr "(>= 3 3)" >>= eval) `shouldBe` Right (Bool True)

    it "evaluates (string=? \"test\" \"test\") to #t" $ do
      (readExpr "(string=? \"test\" \"test\")" >>= eval) `shouldBe` Right (Bool True)

    it "evaluates (string<? \"abc\" \"bba\") to #t" $ do
      (readExpr "(string<? \"abc\" \"bba\")" >>= eval) `shouldBe` Right (Bool True)
