module Scheme
  ( readExpr,
  )
where

import Scheme.Parser (parseExpr)
import Scheme.Types (LispVal (..))
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
