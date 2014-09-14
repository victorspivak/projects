module Examples.Classes1 where

type SHU = Integer -- SHU (Scoville Heat Units)
class Pepper pepper where
    color :: pepper -> String
    pungency :: pepper -> SHU

data Poblano = Poblano
data TrinidadScorpion = TrinidadScorpion

instance Pepper Poblano where
    color Poblano = "green"
    pungency Poblano = 1500
instance Pepper TrinidadScorpion where
    color TrinidadScorpion = "red"
    pungency TrinidadScorpion = 855000

pepperInfo :: Pepper pepper => pepper -> String
pepperInfo pepper = show (pungency pepper) ++ ", " ++ color pepper

class Pepper pepper => Chili pepper where
    kind :: pepper -> String

instance Chili Poblano where
    kind Poblano = "Poblano"

run :: IO ()
run = do
    print ("====================== Examples.Classes1 ===========================")

    putStrLn $ show (pungency trinidad) ++ ", " ++ color trinidad
    print $ pepperInfo trinidad
    print $ kind poblano

    where
        trinidad = TrinidadScorpion
        poblano = Poblano


