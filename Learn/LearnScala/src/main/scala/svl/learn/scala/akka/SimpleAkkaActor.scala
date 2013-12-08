package svl.learn.scala.akka

import akka.actor.{PoisonPill, Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.event.LoggingReceive

/*
* User: victor    Date: 10/26/13   Time: 12:52 AM
*/
object SimpleAkkaActor {
    implicit val timeout = Timeout(1 second)

    def main(args: Array[String]) {
        //To enable LoggingReceive you need to start process with -Dakka.loglevel=DEBUG -Dakka.actor.debug.receive=on or
        //set the following properties in the code
        System.setProperty("akka.loglevel", "DEBUG")
        System.setProperty("akka.actor.debug.receive", "on")

        val system = ActorSystem("HelloSystem")
        val actor = system.actorOf(Props[MyActor], name = "myactor")

        actor ! "Hi"
        //actor ! "quit"
        //actor ! PoisonPill
        actor ! "Oops"
        (actor ? "wait").onSuccess{
            case msg => println(s"Got $msg")
        }

        system.stop(actor)
        system.shutdown()
        system.awaitTermination()
    }

    class MyActor extends Actor{
        def receive = LoggingReceive{
            case "quit" => context.stop(self)
            case "Hi" => println("Hello")
            case "wait" => Thread.sleep(1000)
                sender ! "done"
            case msg:String => println(msg)
        }
    }
}


