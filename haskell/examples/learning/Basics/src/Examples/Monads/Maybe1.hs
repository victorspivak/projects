module Examples.Monads.Maybe1 where

import Data.Char
import Data.String.Utils
import Data.Maybe

{-
    It is like Scala Option

    data Maybe a = Nothing | Just a
        deriving (Eq, Ord)
-}
coefficientFromString :: String -> Maybe Int
coefficientFromString str =
    if isNumber firstChar then Just (digitToInt firstChar) else Nothing
    where firstChar = str !! 0 -- Get symbol with index 0

check :: Maybe Int -> String
check aCoefficient
    | aCoefficient == Nothing = "Invalid string!"
    | otherwise = show aCoefficient

result :: Maybe String -> String
result email = if isNothing email then "Bad email" else "Good!"

run :: IO ()
run = do
    print ("====================== Examples.Monads.Maybe1 ===========================")

    print $ check $ coefficientFromString "0.1"
    print $ check $ coefficientFromString "a10.1"

    print $ result $ Just "me@gmail.com" >>= checkFormat >>= checkDomain

    where
        checkFormat email = if '@' `elem` email then return email else Nothing
        checkDomain email = if email `endsWith` ".com" then return email else Nothing
        endsWith str suffix = endswith suffix str