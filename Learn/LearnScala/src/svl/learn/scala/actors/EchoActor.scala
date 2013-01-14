import java.util.Date
import scala.actors.Actor._

val echoActor = actor {
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

