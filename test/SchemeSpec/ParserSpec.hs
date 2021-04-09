module SchemeSpec.ParserSpec where

import Scheme.Parser
import Scheme.Types (LispVal (..))
import Test.Hspec (describe, it, shouldBe)
import Text.ParserCombinators.Parsec (parse)

spec = do
  describe "parseNumber" $ do
    it "parses 25" $ do
      parse parseNumber "lisp" "25" `shouldBe` Right (Number 25)

    it "parses #d25" $ do
      parse parseNumber "lisp" "#d25" `shouldBe` Right (Number 25)

    it "parses #x19" $ do
      parse parseNumber "lisp" "#x19" `shouldBe` Right (Number 25)

    it "parses #o31" $ do
      parse parseNumber "lisp" "#o31" `shouldBe` Right (Number 25)

    it "parses #b10001" $ do
      parse parseNumber "lisp" "#b11001" `shouldBe` Right (Number 25)

  describe "parseString" $ do
    it "parses string" $ do
      parse parseString "lisp" "\"hello world\"" `shouldBe` Right (String "hello world")

    it "parses quoted string" $ do
      parse parseString "lisp" ("\"hello " ++ ['\\', '\"'] ++ "the" ++ ['\\', '\"'] ++ " world\"") `shouldBe` Right (String $ "hello " ++ ['\"'] ++ "the" ++ ['\"'] ++ " world")

    it "parses string with newline" $ do
      parse parseString "lisp" "\"hello world\n\"" `shouldBe` Right (String "hello world\n")

  describe "parseBool" $ do
    it "parses #t" $ do
      parse parseBool "lisp" "#t" `shouldBe` Right (Bool True)

    it "parses #f" $ do
      parse parseBool "lisp" "#f" `shouldBe` Right (Bool False)

  describe "parseChar" $ do
    it "parses #\\c" $ do
      parse parseChar "lisp" "#\\c" `shouldBe` Right (Character 'c')

    it "parses #\\space" $ do
      parse parseChar "lisp" "#\\space" `shouldBe` Right (Character ' ')

    it "parses #\\newline" $ do
      parse parseChar "lisp" "#\\newline" `shouldBe` Right (Character '\n')

  describe "parseAtom" $ do
    it "parses whatever" $ do
      parse parseAtom "lisp" "whatever" `shouldBe` Right (Atom "whatever")

  describe "parseList" $ do
    it "parses '(a list)'" $ do
      parse parseExpr "lisp" "(a list)" `shouldBe` Right (List [Atom "a", Atom "list"])

  describe "parseDottedList" $ do
    it "parses '(a dotted . list)'" $ do
      parse parseExpr "lisp" "(dotted . list)" `shouldBe` Right (DottedList [Atom "dotted"] (Atom "list"))
