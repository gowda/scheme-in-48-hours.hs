module SchemeSpec where

import Scheme (readExpr)
import Scheme.Types (LispVal (..))
import Test.Hspec (describe, it, shouldBe)

spec = do
  describe "readExpr" $ do
    it "recognizes $" $ do
      readExpr "$" `shouldBe` Right (Atom "$")

    it "recognizes \"string\"" $ do
      readExpr "\"string\"" `shouldBe` Right (String "string")

    it "recognizes quoted string" $ do
      readExpr ("\"" ++ ['\\', '\"', 't', 'h', 'e', '\\', '\"'] ++ " string\"") `shouldBe` Right (String "\"the\" string")

    it "recognizes 25" $ do
      readExpr "25" `shouldBe` Right (Number 25)

    it "recognizes \"symbol\"" $ do
      readExpr "symbol" `shouldBe` Right (Atom "symbol")
