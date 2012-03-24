/*
 * User: Victor    Date: 1/20/12   Time: 9:53 PM
 */

val numNames = Array("zero", "one", "two")
for (value <- numNames)
    println (value)

val greetStrings: Array[String] = new Array[String](3)
greetStrings(0) = "Hello"
greetStrings(1) = ", "
greetStrings(2) = "world!\n"
for (i <- 0 to 2)
    print(greetStrings(i))


// () is apply function and 90 = update

class LikeArray (private var balance_ : Int, private var limit_ : Int) {
    def balance() = balance_
    def limit() = limit_
    def apply (key : Int) = if (key == 1) balance() else limit()
    def apply (key : String) = key match {
        case "Balance" => balance()
        case "Limit" => limit()
        case _ => -999999
    }

    def update (key : String, value_ : Int) = key match {
        case "Balance" => balance_ = value_
        case "Limit" => limit_ = value_
        case _ => throw new IllegalArgumentException ("Invalid Argument: " + key)
    }
}

val likeArray = new LikeArray(5, 10)

Console println likeArray.balance() + "  " + likeArray.limit()

Console println likeArray(1) + "  " + likeArray(2)
Console println likeArray("Balance") + "  " + likeArray("Limit") + "  " + likeArray("Foo")
likeArray("Balance") = 100;
likeArray("Limit") = 1000;
Console println likeArray("Balance") + "  " + likeArray("Limit")




