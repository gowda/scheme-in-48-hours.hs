module SchemeSpec.EvaluatorSpec where

import Scheme (readExpr)
import Scheme.Evaluator (eval)
import Scheme.Types (LispVal (..), extractValue)
import Test.Hspec (describe, it, shouldBe)

evaluateStr :: String -> String
evaluateStr expr = (show . extractValue) $ readExpr expr >>= eval

spec = do
  describe "eval" $ do
    it "evaluates 2 to 2" $ do
      evaluateStr "2" `shouldBe` "2"

    it "evaluates (+ 2 (- 4 1)) to 5" $ do
      evaluateStr "(+ 2 (- 4 1))" `shouldBe` "5"

    it "evaluates (- (+ 4 6 3) 3 5 2) to 3" $ do
      evaluateStr "(- (+ 4 6 3) 3 5 2)" `shouldBe` "3"

    it "evaluates (< 2 3) to #t" $ do
      evaluateStr "(< 2 3)" `shouldBe` "#t"

    it "evaluates (> 2 3) to #f" $ do
      evaluateStr "(> 2 3)" `shouldBe` "#f"

    it "evaluates (>= 3 3) to #t" $ do
      evaluateStr "(>= 3 3)" `shouldBe` "#t"

    it "evaluates (string=? \"test\" \"test\") to #t" $ do
      evaluateStr "(string=? \"test\" \"test\")" `shouldBe` "#t"

    it "evaluates (string<? \"abc\" \"bba\") to #t" $ do
      evaluateStr "(string<? \"abc\" \"bba\")" `shouldBe` "#t"

    it "evaluates (if (> 2 3) \"no\" \"yes\") to \"yes\"" $ do
      evaluateStr "(if (> 2 3) \"no\" \"yes\")" `shouldBe` "\"yes\""

    it "evaluates (if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\") to 9" $ do
      evaluateStr "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")" `shouldBe` "9"

    it "evaluates (car '(2 3 4)) to 2" $ do
      evaluateStr "(car '(2 3 4))" `shouldBe` "2"

    it "evaluates (cdr '(2 3 4)) to (3 4)" $ do
      evaluateStr "(cdr '(2 3 4))" `shouldBe` "(3 4)"

    it "evaluates (car (cdr (cons 2 '(3 4)))) to 3" $ do
      evaluateStr "(car (cdr (cons 2 '(3 4))))" `shouldBe` "3"
