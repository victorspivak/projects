module Examples.Monads.IO1 where

obtainTwoTextsFromUser :: IO String
obtainTwoTextsFromUser = do
    putStrLn "Enter your text, please: "
    firstText <- getLine
    putStrLn "One more, please: "
    secondText <- getLine
    -- without next return the code is not compiled
    -- return is a function and it wraps the result with IO
    return $ "'" ++ firstText ++ "' and '" ++ secondText ++ "'"

run :: IO ()
run = do  -- do makes this code imperative - not lazy and it executes in the right order
    print ("====================== Examples.Monads.IO1 ===========================")

    putStrLn "Input your text, please:"
    lineFromUser <- getLine
    print $ "Hello " ++ lineFromUser

    twoTexts <- obtainTwoTextsFromUser
    putStrLn $ "You said " ++ twoTexts
