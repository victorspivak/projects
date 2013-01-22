object Matching {
    def main(args:Array[String]) {
        def testMatch1 (expr:List[Int]) {
            expr match {
                case List(0, a, b) => println("found it: " + a + " -> " + b)
                case List(0, a, b, _*) => println("found it: " + a + " -> " + b)
                case _ => println("Oops")
            }
        }

        testMatch1(List(0, 1, 1))
        testMatch1(List(0, 1, 1, 1, 0))
    }
}








