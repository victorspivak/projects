module Examples.DataTypes where

data User = User { firstName :: String, lastName :: String, email :: String}

data MyDate = Year String | MonthYear {myDateMonth ::String, myDateYear :: String}

changeEmail :: User -> String -> User
changeEmail user email = user {email = email}
instance Show User where
    show (User f l email) = f ++ " " ++ l ++ " " ++ email

--The following definition uses the short form. The firstName , lastName , email are strings and
-- account , uid are integers

data User1 = User1 { firstName1 , lastName1 , email1 :: String , account1 , uid1 :: Integer }

-- The following definition uses the parametrized type for yearOfBirth -- year.
data User2 d = User2 { firstName2, lastName2, email2 :: String , account2 , uid2 ::Integer, yearOfBirth2 :: d }

instance Show MyDate where
    show (Year y) = y
    show (MonthYear m y) = m ++ "/" ++ y

instance Show a => Show (User2 a) where
    show (User2 f l email ac id y) = f ++ " " ++ l ++ " " ++ email ++ " " ++ (show ac) ++ " " ++ (show id) ++ " " ++ (show y)

changeEmail1 :: (User2 String) -> String -> (User2 String)
changeEmail1 user email = user {email2 = email}
--or
changeEmail2 :: (User2 a) -> String -> (User2 a)
changeEmail2 user email = user {email2 = email}

run :: IO ()
run = do
    print ("====================== DataTypes.Classes ===========================")

    let u1 = User{firstName = "Vic", lastName = "Spi", email = "v@gmail.com"}
    print $ show u1
    let u11 = changeEmail u1 "v@spi.net"
    print $show u11

    let u21 = User2{firstName2 = "Vic", lastName2 = "Spi", email2 = "v@gmail.com", account2 = 111, uid2 = 1111, yearOfBirth2 = "1958"}
    print $ show u21
    let u22 = User2{firstName2 = "Vic", lastName2 = "Spi", email2 = "v@gmail.com", account2 = 111, uid2 = 1111, yearOfBirth2 = Year "1958"}
    print $ show u22
    let u23 = User2{firstName2 = "Vic", lastName2 = "Spi", email2 = "v@gmail.com", account2 = 111, uid2 = 1111, yearOfBirth2 = MonthYear "06" "1958"}
    print $ show u23

    let u211 = changeEmail1 u21 "v@spi.net"
    print $ show u211
    let u231 = changeEmail2 u23 "v@spi.net"
    print $ show u231
    print $ u231 -- it is possible to omit show. It is used automatically
