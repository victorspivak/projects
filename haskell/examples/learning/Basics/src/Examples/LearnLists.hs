module Examples.LearnLists where

import Data.Tuple.Select as T
import Data.Char
import Data.String.Utils

listOfNames :: String -> [String]
listOfNames prefix = [prefix ++ "John", prefix ++ "Anna", prefix ++ "Andrew"]

thisIsAWildAnimal :: String -> Bool
thisIsAWildAnimal name =
    let wildAnimals = ["Bear", "Tiger", "Lion", "Wolf"]
    in name `elem` wildAnimals

checkGooglerBy :: String -> String
checkGooglerBy email = if email `endsWith` "gmail.com" then nameFrom email ++ " is a Googler!" else email
    where   endsWith str suffix = endswith suffix str
            nameFrom fullEmail = takeWhile (/= '@') fullEmail

run :: IO ()
run = do
    print ("====================== Examples.LearnLists ===========================")

    print $ listOfNames "Dear "

    let listOfAnimals = ["Bear", "Tiger", "Lion", "Wolf"]
    print $ length listOfAnimals

    print $ "Lion found: "
    print $ "Lion" `elem` listOfAnimals  -- use backticks to use infix syntax
    print $ elem "Lion" listOfAnimals

    print $ if thisIsAWildAnimal "Cat" then "Yes!" else "No!"

    --To add an element to the begining of the list use :
    let fiveAnimals = "Cat" : listOfAnimals
    print $ fiveAnimals

    let list1 = ["A", "", "B", "C", "", "D"]
    let list2 = filter (\s -> not $ null $ s) list1
    print $ list2

    -- list comprehension
    print [toUpper c | c <- "http"]             -- [OPERATION ELEM | ELEM <- LIST]
    print [toUpper c | c <- "http", c == 't']   -- [OPERATION ELEM | ELEM <- LIST, PREDICATE]
    print [toUpper c | c <- "http", c /= 'h', c /= 'p']     -- /= is != in C and it is &&
    print [toUpper c | c <- "http", c /= 'h' && c /= 'p']
    print [toUpper c | c <- "http", c == 'h' || c == 'p']

    let names = ["James", "Victor", "Denis", "Michael"]
    let simplePrefix = ["Mr. "]
    print [prefix ++ name | name <- names, prefix <- simplePrefix]

    let doublePrefix = ["Mr. ", "sir "]
    print [prefix ++ name | name <- names, prefix <- doublePrefix]

    let cars = ["Mercedes", "BMW", "Bentley", "Audi", "Bentley"]
    print [if car == "Bentley" then "Wow!" else "Good!" | car <- cars]

    print [toUpper c | c <- "http", let hletter = 'h' in c /= hletter]

    print [checkGooglerBy email | email <- ["adam@gmail.com", "bob@yahoo.com", "richard@gmail.com", "elena@yandex.ru", "denis@gmail.com"]]

    -- Ranges
    print $ ['a' .. 'h']
    print $ [1,3..19]
    print $ take 5 [1..]    -- unbound floatRange

    --Tuples
    let person1 = ("Vic", 33)
    print $ person1
    print $ fst person1
    print $ snd person1

    -- To better tuple access install and use tuple module
    print $ T.sel3 ("One", "Two", "Three", "Four")

