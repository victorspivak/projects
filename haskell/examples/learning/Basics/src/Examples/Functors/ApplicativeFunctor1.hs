module Examples.Functors.ApplicativeFunctor1 where

import Control.Applicative

{-

    class Functor f => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b
        (*>) :: f a -> f b -> f b               It drops the first argument
        (<*) :: f a -> f b -> f a

-}

newtype Distance value = Distance value
    deriving Show

add (Distance a) (Distance b) = Distance (a + b)
minus (Distance a) (Distance b) = Distance (a - b)
-- Now we have to define other operations or let's see how AppicativeFunctor helps us

-- let's define it as functors:
instance Functor Distance where
    fmap f (Distance value) = Distance (f value)

instance Applicative Distance where
    Distance f <*> functor = fmap f functor
    pure f = Distance f                         -- or short: pure = Distance

totalSum arg1 arg2 arg3 = arg1 + arg2 + arg3

-- The following example uses Applicative:
obtainTwoTextsFromUser :: IO String
obtainTwoTextsFromUser =
    (++) <$> getFirstText <*> getSecondText
    where
        getFirstText = putStrLn "Enter your text, please: " *> getLine     -- we use here *> because we do not care about putStrLn return
        getSecondText = putStrLn "One more, please: " *> getLine

run :: IO ()
run = do
    print ("====================== Examples.Functors.ApplicativeFunctor1 ===========================")

    print $ Distance 19.78 `add` Distance 1.6
    print $ Distance 19.78 `minus` Distance 1.6

    -- after defining functos for Distance we could use:
    print $ (+) <$> Distance 19.78 <*> Distance 1.6
    print $ (-) <$> Distance 19.78 <*> Distance 1.6
    print $ (*) <$> Distance 20.0 <*> Distance 2.0
                 .WithTabUtils
    print $ totalSum <$> Distance 19.78 <*> Distance 1.6 <*> Distance 289.0

    -- if pure is defined then we could use the following form:
    print $ pure totalSum <*> Distance 19.78 <*> Distance 1.6 <*> Distance 289.0

-- In the following example we got just the last argument
    print $ pure totalSum
        <*> Distance 19.78
        <*> Distance 1.6
        <*> Distance 289.0
        *> Distance 2.0

    -- Monads are functors but functors are not monads
    -- It is better to use functors where functor is enough

    -- The following statement uses monadic:
    print $ [1, 2, 3] >>= \number -> return $ number * 2

    -- The following statement uses functors
    print $ (*2) <$> [1, 2, 3]

