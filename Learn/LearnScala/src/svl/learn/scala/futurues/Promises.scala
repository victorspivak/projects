package svl.learn.scala.futurues

import scala.concurrent.{ future, promise }
import scala.concurrent.ExecutionContext.Implicits.global

object Promises {
    def main(args: Array[String]) {
        val p = promise[String]
        val f = p.future

        p success "Hello"

        f onSuccess{
            case m:String => println(m)
            case _ => println("Oops")
        }


        Thread.sleep(10000)
    }
}
