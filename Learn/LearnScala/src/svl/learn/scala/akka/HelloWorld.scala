package svl.learn.scala.akka

import akka.actor.{ActorSystem, Actor, Props}

class HelloWorld extends Actor {

    override def preStart(): Unit = {
        // create the greeter actor
        val greeter = context.actorOf(Props[Greeter], "greeter")
        // tell it to perform the greeting
        greeter ! Greeter.Greet
    }

    def receive = {
        case Greeter.Done => context.stop(self)
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
    }

}
