object Operators {
    def main(args:Array[String]) {
        val v1 : java.lang.Integer = (1).+(2)
        Console println (1).+(2)
        Console println (2.0).unary_-

        Console println ("== uses equals: " + (5 == 5L))
        val hello1 = "Hello"
        val hello2 = "He" + "llo"

        Console println ("== uses equals: " + (hello1 == hello2))
        Console println ("eq is like java ==: " + (hello1 eq hello2))
    }
}







