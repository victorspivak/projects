package svl.learn.scala.continuations

import scala.util.continuations.suspendable
import scala.util.continuations._

/*
 * User: victor    Date: 10/8/13   Time: 1:02 AM
 */
object ConsumerProducer {

    var producerCont : (Unit => Unit) = null
    var consumerCont : (Int => Unit) = null

    def produce(i : Int) : Unit @suspendable =
        shift {
            (k : Unit => Unit) => {
                producerCont = k
                consumerCont(i)
                consumerCont = null
            }
        }

    def consume : Int @suspendable =
        shift {
            (k : Int => Unit) => {
                consumerCont = k
                if (producerCont != null)
                    producerCont()
                producerCont = null
            }
        }

    def main(args: Array[String]) {
        reset {
            println("Consuming: "+consume)
            println("Consuming: "+consume * 100)
            println("Consuming: "+consume)
        }

        reset {
            println("Producing: 1")
            produce(1)
            println("Producing: 2")
            produce(2)
            println("Producing: 3")
            produce(3)
        }
    }
}
