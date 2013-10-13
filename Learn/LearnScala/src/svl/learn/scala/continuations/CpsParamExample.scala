package svl.learn.scala.continuations

import scala.util.continuations._

/*
 * User: victor    Date: 10/12/13   Time: 2:36 AM
 */
object CpsParamExample {
    def is123(n:Int):Boolean = {
        reset {
            is123sub(n)
        }
    }

    def is123sub(n:Int):String @cpsParam[String,Boolean] = {
        shift { k : (Int=>String) =>
            k(n) == "123"
        }.toString
    }

    def main(args: Array[String]) {
        println(is123(100))
    }
}
