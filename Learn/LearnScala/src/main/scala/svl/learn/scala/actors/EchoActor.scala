import actors.Actor
import java.util.Date
import scala.actors.Actor._

object EchoActor {
    def main(args:Array[String]) {
        val echoActor: Actor = actor {
            while (true) {
                receive {
                    case msg =>
                        println("received message: "+ msg)
                }
            }
        }

        echoActor ! "hi there"
        echoActor ! 15

        val typedActor = actor {
            while (true) {
                receive {
                    case x: Int => println("Got an Int: "+ x)
                    case s: String => println("Got a String: " + s)
                    case o => println("Unknown: " + o)
                }
            }
        }

        typedActor ! "Hello"
        typedActor ! 25
        typedActor ! new Date
    }
}


