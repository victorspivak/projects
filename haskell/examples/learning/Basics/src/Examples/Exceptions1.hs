module Examples.Exceptions1 where

import Control.Exception

{-
    The tryToOpenFile2 function uses catch. It is readable when we use infix form
    Function catch has the following signature:
        catch :: Exception e => IO a -> (e -> IO a) -> IO a
-}
tryToOpenFile1 :: FilePath -> IO String
tryToOpenFile1 path =
    readFile path `catch` possibleErrors
    where
        possibleErrors :: IOException -> IO String
        possibleErrors error = return $ show error

{-
    The tryToOpenFile2 function uses handle. It is like catch only with different argument order
-}
tryToOpenFile2 :: FilePath -> IO String
tryToOpenFile2 path =
    handle possibleErrors (readFile path) -- то же самое, но наоборот...
    where
        possibleErrors :: IOException -> IO String
        possibleErrors error = return "Aaaaa!!! Please check file."


run :: IO ()
run = do
    print ("====================== Examples.Exceptions1 ===========================")

    let path = "Users/victorspivak/test.c"

    fileContent1 <- tryToOpenFile1 path
    putStrLn fileContent1

    fileContent2 <- tryToOpenFile2 path
    putStrLn fileContent2

    fileContent3 <- try $ readFile path :: IO (Either IOException String)
    case fileContent3 of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right content -> putStrLn content

    {-
        We must use evaluate in the following statement because pure function might not throw exception
        The evaluate has the following signature:
            evaluate :: a -> IO a
    -}
    result <- try $ evaluate $ 2 `div` 0 :: IO (Either SomeException Integer)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right value -> print value