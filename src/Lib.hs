module Lib
  ( readExpr,
  )
where

import Parser (parseExpr)
import Text.ParserCombinators.Parsec (parse)
import Types (LispVal (..))

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
