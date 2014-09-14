module Examples.Classes3 where

newtype SHU = SHU Integer

instance Show SHU where
    show (SHU value) = show value

class Pepper pepper where
--    simple :: pepper -- It is a const
    color :: pepper -> String
    pungency :: pepper -> SHU
    name :: pepper -> String

data PeperData = PeperData {pdName, pdColor::String, pdPungency::SHU}

instance Pepper PeperData where
--    simple = "ancho"
    color (PeperData n c p) = c
    pungency (PeperData n c p) = p
    name (PeperData n c p) = n

pepperInfo :: Pepper pepper => pepper -> String
pepperInfo pepper = show (pungency pepper) ++ ", " ++ color pepper

run :: IO ()
run = do
    print ("====================== Examples.Classes3 ===========================")

    let poblano = PeperData {pdName = "Poblano", pdColor = "green", pdPungency = SHU 1500}
    let trinidad = PeperData {pdName = "TrinidadScorpion", pdColor = "red", pdPungency = SHU 855000}

    print $ color poblano
    print $ pepperInfo trinidad


