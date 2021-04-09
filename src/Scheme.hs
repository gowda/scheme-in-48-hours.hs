module Scheme
  ( readExpr,
  )
where

import Control.Monad.Error
import Scheme.Parser (parseExpr)
import Scheme.Types (LispError (..), LispVal (..), ThrowsError (..))
import Text.ParserCombinators.Parsec (parse)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
