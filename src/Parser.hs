module Parser
  ( parseBool,
    parseAtom,
    parseChar,
    parseString,
    parseNumber,
    parseList,
    parseDottedList,
    parseExpr,
  )
where

import Control.Monad (liftM)
import Data.List (intercalate)
import Numeric (readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)
import Types (LispVal (..))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do
  char '\\'
  x <- oneOf "\\\""
  case x of
    '\\' -> do return [x]
    '"' -> do return [x]
    't' -> do return "\t"
    'n' -> do return "\n"
    'r' -> do return "\r"

parseChar :: Parser LispVal
parseChar = do
  try $ string "#\\"
  (parseCharName <|> anyChar) >>= return . Character

parseCharName = do
  x <- try (string "space" <|> string "newline")
  case x of
    "space" -> do return ' '
    "newline" -> do return '\n'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ many1 (noneOf "\"\\") <|> escapedChars
  char '"'
  return $ String (concat x)

parseBool :: Parser LispVal
parseBool = do
  string "#"
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom (first : rest)

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= return . Number . read

parseDigital2 :: Parser LispVal
parseDigital2 = do
  try $ string "#d"
  many1 digit >>= return . Number . read

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  many1 hexDigit >>= return . Number . hex2dig

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  many1 octDigit >>= return . Number . oct2dig

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  many1 (oneOf "01") >>= return . Number . bin2dig

hex2dig x = fst $ readHex x !! 0

oct2dig x = fst $ readOct x !! 0

bin2dig = bin2dig' 0

bin2dig' digint "" = digint
bin2dig' digint (x : xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1)
   in bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> try parseChar
    <|> try parseBool
    <|> try parseNumber
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
