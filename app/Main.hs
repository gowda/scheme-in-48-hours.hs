module Main where

import Scheme (readExpr)
import Scheme.Evaluator (eval)
import System.Environment

main :: IO ()
main = do
  getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
