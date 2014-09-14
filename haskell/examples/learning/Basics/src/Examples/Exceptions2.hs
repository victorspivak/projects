{-# LANGUAGE DeriveDataTypeable #-}

--The above statement is required for user defined exceptions

module Examples.Exceptions2 where

import Control.Exception
import Data.String.Utils
import Data.Typeable

type Repo = String

data InvalidRepository = InvalidRepository Repo deriving (Show, Typeable)

instance Exception InvalidRepository

extractProtocol :: String -> String
extractProtocol path =
    if path `startsWith` "git" || path `startsWith` "ssh"
    then takeWhile (/= ':') path
    else throw $ InvalidRepository path
    where startsWith url prefix = startswith prefix url

run :: IO ()
run = do
    print ("====================== Examples.Exceptions2 ===========================")

    result <- try $ evaluate $ extractProtocol "ss://ul@sch/proj.git" :: IO (Either SomeException String)
    case result of
        Left exception -> putStrLn $ "Fault: " ++ show exception
        Right protocol -> putStrLn protocol

