package svl.learn.scala.akka

import akka.actor.{PoisonPill, Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

object MultipleActors {
    implicit val timeout = Timeout(1 second)

    val system = ActorSystem("HelloSystem")

    def main(args: Array[String]) {

        val a1 = runActor("a1")
        val a2 = runActor("a2")
        val a3 = runActor("a3")

        println(a1.isTerminated)
        Thread.sleep(2000)
        println(a1.isTerminated)

        finish()
    }


    def runActor(name:String) = {
        val actor = system.actorOf(Props[MyActor], name)
        actor ! "wait"
        actor ! PoisonPill
        actor
    }

    def finish (){
        system.shutdown()
        system.awaitTermination()
    }

    class MyActor extends Actor{
        def receive = {
            case "wait" => Thread.sleep(1000)
            case msg:String => println(msg)
        }
    }
}


