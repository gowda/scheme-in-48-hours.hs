module SchemeSpec where

import Scheme (readExpr)
import Scheme.Types (LispVal (..))
import Test.Hspec (describe, it, shouldBe)

spec = do
  describe "readExpr" $ do
    it "recognizes $" $ do
      readExpr "$" `shouldBe` (Atom "$")

    it "recognizes \"string\"" $ do
      readExpr "\"string\"" `shouldBe` (String "string")

    it "recognizes quoted string" $ do
      readExpr ("\"" ++ ['\\', '\"', 't', 'h', 'e', '\\', '\"'] ++ " string\"") `shouldBe` (String "\"the\" string")

    it "recognizes 25" $ do
      readExpr "25" `shouldBe` (Number 25)

    it "recognizes \"symbol\"" $ do
      readExpr "symbol" `shouldBe` (Atom "symbol")
