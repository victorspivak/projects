package svl.learn.scala.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

object AkkaActorsBecome {
    implicit val timeout = Timeout(1 second)

    def main(args: Array[String]) {
        val system = ActorSystem("HelloSystem")
        val actor = system.actorOf(Props[MyActor], name = "myactor")

        actor ! "talk"
        actor ! "talk"
        actor ! "talk"
        actor ! "talk"
        actor ! "talk"

      //It is sync message
        (actor ? "count").map(println)

        system.stop(actor)
        system.shutdown()
        system.awaitTermination()
    }

    class MyActor extends Actor{
        def receive = getBehavior(0)
        def getBehavior(counter:Int): Receive = {
            case "talk" => context become(getBehavior(counter + 1), true)
            case "count" => sender ! counter
        }
    }
}
