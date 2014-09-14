module Examples.Functors.Functor1 where

import Data.Char
import Data.Functor

{-
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
-}

data Year value = Year value
    deriving Show

instance Functor Year where
    fmap f (Year value) = Year (f value)

increase :: Int -> Int
increase v = v + 1

run :: IO ()
run = do
    print ("====================== Examples.Functors.Functor1 ===========================")

    print $ fmap toLower ['A'..'Z']
    print $ fmap digitToInt ['1'..'9']

    let year = Year 2014
    print $ fmap increase year

    print $ increase <$> year       --infix form. it is the same as fmap