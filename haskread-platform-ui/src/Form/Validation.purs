module Form.Validation where

import Prelude

import Data.Either (Either(..))
import Data.String as String

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | PasswordsDidNotMatched

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  PasswordsDidNotMatched -> "Passwords did not match"

required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong

emailFormat :: String -> Either FormError String
emailFormat = check (String.contains (String.Pattern "@")) InvalidEmail

passwordsEqual :: String -> String -> Either FormError String
passwordsEqual p1 = check (\x -> x==p1) PasswordsDidNotMatched

check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err