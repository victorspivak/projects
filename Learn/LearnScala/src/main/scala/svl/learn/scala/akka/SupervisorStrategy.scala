package svl.learn.scala.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.IOException
import akka.actor.SupervisorStrategy.{Escalate, Stop, Restart}
import akka.actor.OneForOneStrategy
import scala.concurrent.Await

object SupervisorStrategy {
  implicit val timeout = Timeout(1 second)

  def main(args: Array[String]) {
    val system = ActorSystem("HelloSystem")
    val actor = system.actorOf(Props[MainActor], name = "mainActor")

    val worker1 = Await.result(actor ? "worker1", 1 second).asInstanceOf[ActorRef]
    val worker2 = Await.result(actor ? "worker2", 1 second).asInstanceOf[ActorRef]

    (worker1 ? "greet").map(println)
    worker1 ! "fail1"
    (worker1 ? "greet").map(println)

    (worker2 ? "greet").map(println)
    worker2 ! "fail2"
    (worker2 ? "greet").map(println)

    (worker1 ? "greet").map(println)
    worker1 ! "exit"
    (worker1 ? "greet").map(println)

    Thread.sleep(3000)

    system.shutdown()
    system.awaitTermination()
  }

  class MainActor extends Actor{
    override def supervisorStrategy = OneForOneStrategy(){
      case _:IOException => Escalate
      case _:ActorKilledException => Stop
      case _:RuntimeException =>
        println("Restarting... " + sender)
        Restart
    }

    val worker1 = context.actorOf(Props[MyActor], "worker1")
    val worker2 = context.actorOf(Props[MyActor], "worker2")

    def receive = {
      case "worker1" => sender ! worker1
      case "worker2" => sender ! worker2
    }
  }

  class MyActor extends Actor{
    def receive = {
      case "greet" => sender ! "Hello from: " + self
      case "exit" => context.stop(self)
      case "fail1" => throw new RuntimeException("kuku")
      case "fail2" => throw new IOException("kuku")
    }
  }
}
