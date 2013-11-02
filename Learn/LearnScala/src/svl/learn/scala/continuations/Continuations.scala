package svl.learn.scala.continuations

import scala.util.continuations._

/*
 * User: victor    Date: 10/7/13   Time: 9:50 PM
 */
object Continuations {
    def main(args: Array[String]) {
        val r = reset {
            shift { k: (Int=>Int) =>
                k(7)
            } + 1
        }

        println(r)
        println("==============================================")

        reset {
            println(1)
            shift { (cont: Unit => Unit) => }
            println(2)
        }
        //prints: 1
        println("==============================================")

        reset {
            println(1)
            shift { (cont: Unit => Unit) =>
                println(2)
            }
            println(3)
        }
        //prints: 1 2
        println("==============================================")

        reset {
            println(1)
            shift { (cont: Unit => Unit) =>
                cont()
                println(2)
            }
            println(3)
        }
        //prints: 1 3 2
        println("==============================================")

        reset {
            println(1)
            shift { (cont: Unit => Unit) =>
                cont()
                cont()
                println(2)
            }
            println(3)
        }
        //prints: 1 3 3 2
        println("==============================================")
    }
}
