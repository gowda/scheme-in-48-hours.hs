module Main where

import Control.Monad (liftM)
import Scheme (readExpr)
import Scheme.Evaluator (eval)
import Scheme.Types (extractValue, trapError)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval

  putStrLn $ extractValue $ trapError evaled
