module LibSpec where

import Lib
import Test.Hspec (describe, it, shouldBe)
import Types (LispVal (..))

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
