module Examples.HighOrderFunctions where

import Data.Char
import Data.String.Utils

addPrefix :: String -> String
addPrefix url =
    if url `startsWith` prefix then url else prefix ++ url
    where prefix = "http://"

startsWith url prefix = startswith prefix url

encodeAllSpaces = replace " " "%20" 

makeItLowerCase = map toLower

formatUrl = addPrefix . encodeAllSpaces . makeItLowerCase

run :: IO ()
run = do
    print ("====================== Examples.HighOrderFunctions ===========================")
    do
        let url = "www.SITE.com/test me/Start page" 
        putStrLn (addPrefix (encodeAllSpaces (makeItLowerCase url)))
    do
        let url = "www.GooGle.Net" 
        putStrLn (addPrefix (encodeAllSpaces (makeItLowerCase url)))
    
    let url = "www.victor spivak.net"
    print (formatUrl "www.victor spivak.net")    

    -- . is function composition
    -- It is the sa me is addPrefix (encodeAllSpaces (makeItLowerCase url))
    let res1 = (addPrefix . encodeAllSpaces . makeItLowerCase) url 
    print (res1)    
    -- $ is function application
    -- it is the same as addPrefix (encodeAllSpaces (makeItLowerCase url))
    let res2 = addPrefix $ encodeAllSpaces $ makeItLowerCase url 
    print (res2)    

