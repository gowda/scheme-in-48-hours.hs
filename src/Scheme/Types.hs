module Scheme.Types
  ( LispVal (..),
  )
where

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char

instance Eq LispVal where
  Bool x == Bool y = x == y
  Number x == Number y = x == y
  String x == String y = x == y
  Atom x == Atom y = x == y
  Character x == Character y = x == y
  List [x] == List [y] = x == y
  List (x : xs) == List (y : ys) = x == y
  DottedList xs x == DottedList ys y = x == y
  _ == _ = False

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Number contents) = show contents
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Character x) = show x
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
