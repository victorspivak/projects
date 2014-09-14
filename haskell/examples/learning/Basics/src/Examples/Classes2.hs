module Examples.Classes2 where

type SHU = Integer

class Pepper pepper where
    simple :: pepper -- It is a const
    color :: pepper -> String
    pungency :: pepper -> SHU
    name :: pepper -> String

data Poblano = Poblano String -- Compare with C1 - it has constructor with parameter

instance Pepper Poblano where
    simple = Poblano "ancho"
    color (Poblano name) = "green"
    pungency (Poblano name) = 1500
    name (Poblano name) = name

-- newtype is like type classes in Scala
newtype IPAddress = IP String deriving Show

run :: IO ()
run = do
    print ("====================== Examples.Classes2 ===========================")

    putStrLn $ name (simple :: Poblano) -- access to const

