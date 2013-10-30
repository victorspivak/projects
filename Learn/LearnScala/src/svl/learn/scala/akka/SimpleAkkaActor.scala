package svl.learn.scala.akka

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
/*
* User: victor    Date: 10/26/13   Time: 12:52 AM
*/
object SimpleAkkaActor {
    implicit val timeout = Timeout(1 second)

    def main(args: Array[String]) {
        val system = ActorSystem("HelloSystem")
        val actor = system.actorOf(Props[MyActor], name = "myactor")

        actor ! "Hi"
        actor ! "Oops"
        (actor ? "wait").onSuccess{
            case msg => println(s"Got $msg")
        }

        Thread.sleep(2000)
        system.stop(actor)
        system.shutdown()
    }
}

class MyActor extends Actor{
    def receive = {
        case "Hi" => println("Hello")
        case "wait" => Thread.sleep(1000)
                        sender ! "done"
        case msg:String => println(msg)
    }
}

