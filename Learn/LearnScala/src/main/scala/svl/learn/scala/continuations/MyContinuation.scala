package svl.learn.scala.continuations

import scala.util.continuations.suspendable
import scala.util.continuations._

/*
 * User: victor    Date: 10/8/13   Time: 1:09 AM
 */
object MyContinuation {
    def waiting(timeout:Int, symbol:Char = '.') {
        (1 to timeout) foreach{i=>
            print(symbol)
            Thread.sleep(1000)
        }
        println()
    }

    def main(args: Array[String]) {
        var func : (Unit => Unit) = null

        def doIt(i : Int) : Unit @cpsParam[Unit, Unit] =
            shift {(cont : Unit => Unit) =>
                    waiting(i)
                    println(2)
                    func = cont
            }

        reset {
            println(1)
            doIt(5)
            waiting(4, '!')
            println(3)
        }
        println("Before calling cont1")
        waiting(7, '?')
        func()

        println("==============================================")

    }
}
