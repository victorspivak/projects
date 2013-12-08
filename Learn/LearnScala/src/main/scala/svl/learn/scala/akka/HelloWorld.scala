package svl.learn.scala.akka

import akka.actor.{ActorRef, ActorSystem, Actor, Props}
import scala.actors.{PoisonPill}

class HelloWorld extends Actor {
    var greeter:ActorRef = null
    override def preStart(): Unit = {
        greeter = context.actorOf(Props[Greeter], "greeter")
        greeter ! Greeter.Greet
    }

    override def postStop() = {
        println(s"greeter is ${greeter.isTerminated}")
        context.stop(greeter)
        println(s"greeter is ${greeter.isTerminated}")
    }

    def receive = {
        case Greeter.Done =>
            println("*> Done")
            println(s"greeter is ${greeter.isTerminated}")
    }
}

object Greeter {
    case object Greet
    case object Done
}

class Greeter extends Actor {
    def receive = {
        case Greeter.Greet =>
            println("Hello World!")
            sender ! Greeter.Done
    }
}

object Driver {
    def main(args: Array[String]) {
        val system = ActorSystem("Main")
        val ac = system.actorOf(Props[HelloWorld])

        Thread.sleep(1000)
        println(s"ac is ${ac.isTerminated}")

        system.stop(ac)
        Thread.sleep(1000)
        println(s"ac is ${ac.isTerminated}")

        system.shutdown()
        system.awaitTermination()
    }
}
