package svl.learn.scala.akka

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.IOException
import scala.concurrent.Await
import scala.language.postfixOps

object ActorMonitor {
  implicit val timeout = Timeout(1 second)

  def main(args: Array[String]) {
    val system = ActorSystem("HelloSystem")
    val actor = system.actorOf(Props[MainActor], name = "mainActor")

    val worker1 = Await.result(actor ? "worker1", 1 second).asInstanceOf[ActorRef]
    val worker2 = Await.result(actor ? "worker2", 1 second).asInstanceOf[ActorRef]

    (worker1 ? "greet").map(println)
    val r = (worker1 ? "fail1").onFailure {
      case e: Exception => println("---------------------> " + e)
    }
    (worker1 ? "greet").map(println)

    (worker2 ? "greet").map(println)
    worker2 ! "fail2"
    (worker2 ? "greet").map(println)

    (worker1 ? "greet").map(println)
    worker1 ! "exit"
    (worker1 ? "greet").map(println)

    worker2 ! "exit"

    Thread.sleep(3000)

    system.shutdown()
    system.awaitTermination()
  }

  class MainActor extends Actor {
    val worker1 = context.actorOf(Props[MyActor], "worker1")
    val worker2 = context.actorOf(Props[MyActor], "worker2")

    context.watch(worker1)
    context.watch(worker2)

    def receive = {
      case "worker1" => sender ! worker1
      case "worker2" => sender ! worker2
      case Terminated(`worker1`) => println(">>>>>>>>>>>>>>>>>>>>> worker1 is terminated")
      case Terminated(`worker2`) => println(">>>>>>>>>>>>>>>>>>>>> worker2 is terminated")
    }
  }

  class MyActor extends Actor {
    def receive = {
      case "greet" => sender ! "Hello from: " + self
      case "exit" => context.stop(self)
      case "fail1" => throw new RuntimeException("kuku")
      case "fail2" => throw new IOException("kuku")
    }
  }

}
