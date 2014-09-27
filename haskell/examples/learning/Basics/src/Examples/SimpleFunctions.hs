module Examples.SimpleFunctions where
{- 
	A function with condition logic coould be implemented
		-- using if/else statement
		-- using several implementation depends of the params values
		-- using guard
-}

<<<<<<< HEAD
import Debug.Trace

=======
>>>>>>> 5fa8cc91ff20d5a886e215dc93fc8bc35687afe8
indicate1 :: String -> String
indicate1 address = 
    if address == "127.0.0.1" then "localhost" else address

indicate2 :: String -> String
indicate2 "127.0.0.1" = "localhost"
indicate2 address = address

indicate3 :: String -> String
indicate3 address
    | address == "127.0.0.1" = "localhost"
    | null address = "empty IP"
    | otherwise = address

simpleDoubler value = 2 * value

-- An example of using "where" local expression
prepareLength1 :: Double -> Double
prepareLength1 line =
    line * coefficient + correction
    where coefficient = 0.4959
          correction = 0.1

-- An example of using "let"
-- let bindings in expression
prepareLength2 :: Double -> Double
prepareLength2 line =

    let coefficient = 12.4959
        correction = 0.0012
        showLine = show line
        msg = "Called prepareLength2 with: " ++ showLine
    in
    (trace msg line) * coefficient - correction

{-  where vs. let:
    where could be only at the end of the function and it is visible for entire function
    let  couild be many times and it is vissible only for associated statements
-}

run :: IO ()
run = do
    print ("====================== Examples.SimpleFunctions ===========================")
    print (take 2 (replicate 100 "127.0.0.1"))
    print (take 5 (repeat "127.0.0.1"))
    print (simpleDoubler 10)
    print (indicate1 "11.11.11.11")
    print (indicate1 "127.0.0.1")
    print (indicate2 "11.11.11.11")
    print (indicate2 "127.0.0.1")
    print (indicate3 "")
    print (indicate3 "11.11.11.11")
    print (indicate3 "127.0.0.1")
    print (prepareLength1 10.0)
    print (prepareLength2 10.0)
