module Examples.Recurssions1 where

fuct :: Integer -> Integer
fuct 1 = 1
fuct 2 = 2
fuct n = n * fuct (n - 1)


run :: IO ()
run = do
    print ("====================== Examples.Recurssions1 ===========================")

    print $ fuct 5
