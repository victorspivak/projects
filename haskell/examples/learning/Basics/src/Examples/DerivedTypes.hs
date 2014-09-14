module Examples.DerivedTypes where

-- deriving Show produces a default show implementation
data IPAddress = IP String | Host String deriving Show

{--------------------------------------------------------------------------------------------
    There are several classes that could be derived fromEnum:
    Eq - supports:
        (==)
        (/=)
    Ord - supports:
        (<)
        (<=)
        (>)
        (>=)
        max
        min
    Enum - makes a class with null constructors enumerable
    Bounded - allows to apply minBound and maxBound functions.  These functions are applied to the type
    Read and Show - support serialization

    There are a couple more classes that could be derived from
---------------------------------------------------------------------------------------------}

data IPAddress1 = IP1 String deriving (Show, Eq, Ord)

data TransportLayer = TCP | UDP | SCTP | DCCP | SPX deriving (Show, Enum, Bounded)
descriptionOf :: TransportLayer -> String
descriptionOf protocol =
    case protocol of
        TCP -> "Transmission Control Protocol"
        UDP -> "User Datagram Protocol"
        SCTP -> "Stream Control Transmission Protocol"
        DCCP -> "Datagram Congestion Control Protocol"
        SPX -> "Sequenced Packet Exchange"

data User = User { firstName , lastName , email , yearOfBirth :: String , account , uid :: Integer} deriving (Show, Read, Eq)

run :: IO ()
run = do
    print ("====================== Examples.DerivedTypes ===========================")

    print $ IP "127.0.0.1"
    print $ Host "127.0.0.1"

    let a1 = IP1 "127.0.0.1"
    let a2 = IP1 "127.0.0.2"

    print $ a1 == a2
    print $ a1 < a2
    print $ a1

    print [descriptionOf protocol | protocol <- [TCP .. SCTP]]
    -- Space between TCP and .. is required
    print [descriptionOf protocol | protocol <- [TCP ..]]

    print $ "Int value: " ++ show (minBound :: Int) ++ " --> " ++ show (maxBound :: Int)
    print $ "TransportLayer value: " ++ show (minBound :: TransportLayer) ++ " --> " ++ show (maxBound :: TransportLayer)

    let
        object = User { firstName = "Denis" , lastName = "Shevchenko" , email = "me@dshevchenko.biz" , yearOfBirth = "1981" ,
            account = 1234567890, uid = 123}
        serializedObject = show object
        deserializedObject = read serializedObject
    print $ object == deserializedObject
