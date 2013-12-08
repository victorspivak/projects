package svl.learn.scala.matching

/*
 * User: victor    Date: 11/28/13   Time: 4:03 PM
 */
object Unapply {

    def test1() = {
        object Prime {
            def unapply(x: Int): Option[Int] = if ((2 until x).forall(x % _ != 0)) Some(x) else None
        }

        def printIfPrime(n:Int) = {
            n match {
                case Prime(x) => println(s"$n is Prime. We like prime numbers!")
                case _=> println(s"$n is NOT Prime.")
            }
        }

        printIfPrime(5)
        printIfPrime(15)
        printIfPrime(31)
    }

    def main(args: Array[String]) {
        test1()
    }

}
