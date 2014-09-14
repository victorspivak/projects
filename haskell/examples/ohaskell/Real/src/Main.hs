import Helpers
import Data.Text as T

name = "Victor"
format = T.unpack . T.toUpper . T.pack
main = putStrLn (hello (format name))
