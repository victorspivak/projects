module Examples.Module1 (
                        calibrate,    --this list shows what could be imported
                        graduate,
                        run,
                        Color (Red, Green, Blue)    -- for types we have to specify also all constructors as tuple
                        )
                        where

coefficient :: Double
coefficient = 0.99874

calibrate length = length * coefficient
graduate length = length / coefficient

data Color = Red | Green | Blue deriving Show

run :: IO ()
run = do
    print ("====================== Examples.Module1 ===========================")

