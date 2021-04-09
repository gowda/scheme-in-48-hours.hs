module Main where

import Evaluator (eval)
import Lib
import System.Environment

main :: IO ()
main = do
  getArgs >>= putStrLn . show . eval . readExpr . (!! 0)
