module Examples.SimpleClassesAndGenerics where

data IPAddress = IPAddress String

--There is a standard Class Show:
--class Show a where
--    show :: a -> String

instance Show IPAddress where
    show (IPAddress address) =
        if address == "127.0.0.1" then "localhost" else address

--Constructor might have different name than data:
data IPAddressX = IP String | Host String

instance Show IPAddressX where
    show (IP address) = address

    show (Host address) =
        if address == "127.0.0.1" then "localhost" else address

{-
*****************************************************************************************************
Bellow functions with generics
-}
--There is a standard class Eq. It has two methods:
--(==) :: a -> a -> Bool
--(/=) :: a -> a -> Bool

nothing1 :: a -> a
nothing1 val = val

-- The nothing2 will not compiled without next Instance
instance Eq IPAddressX where
    IP x == IP y = x == y
    Host x == Host y = x == y
    _ == _ = False

nothing2 :: Eq a => a -> a
nothing2 val = val

nothing3 :: (Show a, Show b, Eq b) => a -> b -> String
nothing3 value1 value2 = show value1 ++ " ==> " ++ show value2

-- Null constructors could be used for enums:
data TransportLayer = TCP | UDP | SCTP | DCCP | SPX

descriptionOf :: TransportLayer -> String
descriptionOf protocol = case protocol of
    TCP -> "Transmission Control Protocol"
    UDP -> "User Datagram Protocol"
    SCTP -> "Stream Control Transmission Protocol"
    DCCP -> "Datagram Congestion Control Protocol"
    SPX -> "Sequenced Packet Exchange"

run :: IO ()
run = do
    print ("====================== Examples.SimpleClassesAndGenerics ===========================")
    print $ show 3
    print $ show (IPAddress "127.0.0.1")
    print $ show $ IPAddress "127.0.0.1"

    print $ show $ IP "127.0.0.1"
    print $ show $ Host "127.0.0.1"

    print $ nothing1 $ Host "127.0.0.1"
    print $ nothing2 $ Host "127.0.0.1"

    print $
        let a1 = IP $ "127.0.0.1"
            a2 = Host $ "127.0.0.1"
        in nothing3 a1 a2

    print $ descriptionOf TCP
