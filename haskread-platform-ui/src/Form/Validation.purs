module Form.Validation where

import Prelude


import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername
  | InvalidAvatar

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"
  InvalidAvatar -> "Invalid image URL"

required :: forall a. Eq a => Monoid a => a -> Either FormError a
required = check (_ /= mempty) Required

minLength :: Int -> String -> Either FormError String
minLength n = check (\str -> String.length str > n) TooShort

maxLength :: Int -> String -> Either FormError String
maxLength n = check (\str -> String.length str <= n) TooLong

emailFormat :: String -> Either FormError String
emailFormat = check (String.contains (String.Pattern "@")) InvalidEmail

-- usernameFormat :: String -> Either FormError Username
-- usernameFormat = note InvalidUsername <<< Username.parse

-- avatarFormat :: String -> Either FormError Avatar
-- avatarFormat = note InvalidAvatar <<< Avatar.parse

check :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
check f err a
  | f a = Right a
  | otherwise = Left err

-- toOptional
--   :: forall a b
--    . Monoid a
--   => Eq a
--   => (a -> Either FormError b)
--   -> (a -> Either FormError (Maybe b))
-- toOptional k = \value ->
--   if value == mempty then
--     Right Nothing
--   else
--     map Just $ k value