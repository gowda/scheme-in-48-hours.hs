{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Types
  ( LispVal (..),
    LispError (..),
    ThrowsError (..),
    Unpacker (..),
    trapError,
    extractValue,
  )
where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError (..))

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

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseError) = "Parser error at " ++ show parseError

instance Eq LispError where
  (BadSpecialForm message1 form1) == (BadSpecialForm message2 form2) = message1 == message2 && form1 == form2

instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
