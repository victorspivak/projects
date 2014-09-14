module Examples.Monads.Monads1 where

{-
    Class Monad has the following definition:

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b         it is named bind
        (>>) :: m a -> m b -> m b                 it is named then
        return :: a -> m a                        It is return and it wraps a value with monad
        fail :: String -> m a


-}

import Data.Char

toLowerCase = return . toLower
underlineSpaces char = return (if char == ' ' then '_' else char)
toRealNumbers = return . digitToInt

run :: IO ()
run = do
    print ("====================== Examples.Monads.Monads1 ===========================")

    {-
        The following code
            putStrLn "Enter your name:"
            text <- getLine
            putStrLn $ "Hello '" ++ text ++ "'"
        is monadic:
    -}

    -- putStrLn "Enter your name:" >> getLine >>= \text -> putStrLn $ "Hello '" ++ text ++ "'"

    --String is monad and we could use monadic interface:
    print $ "Hello World" >>= toLowerCase >>= underlineSpaces

    --In the following example we change the type from Char to Integer
    print $ "1234567890" >>= toRealNumbers


