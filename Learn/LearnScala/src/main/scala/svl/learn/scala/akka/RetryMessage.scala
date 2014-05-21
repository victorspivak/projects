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
import scala.language.postfixOps

object RetryMessage {
  implicit val timeout = Timeout(1 second)

  def main(args: Array[String]) {
    val system = ActorSystem("HelloSystem")
    val actor = system.actorOf(Props[MainActor], name = "mainActor")

    val worker = Await.result(actor ? "worker1", 1 second).asInstanceOf[ActorRef]

    (worker ? "greet").map(println)
    (worker ? "fail1").map(println)
    (worker ? "fail1").map(println)
    (worker ? "fail1").map(println)
    (worker ? "fail1").map(println)

    Thread.sleep(3000)

    system.shutdown()
    system.awaitTermination()
  }

  class MainActor extends Actor {
    override def supervisorStrategy = OneForOneStrategy() {
      case _: IOException => Escalate
      case _: ActorKilledException => Stop
      case _: RuntimeException =>
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

  class MyActor extends Actor {
    var attempt = 0

    def receive = {
      case "greet" => sender ! "Hello from: " + self
      case "exit" => context.stop(self)
      case "fail1" =>
        attempt += 1
        if ((attempt % 2) == 0)
          throw new RuntimeException("kuku")
        else
          sender ! "I am fine"
      case "fail2" => throw new IOException("kuku")
    }

    override def preRestart(reason: Throwable, message: Option[Any]) {
      //message foreach { self forward _ }
      message map { msg =>
        println("retry failed: " + msg)
        self forward msg
      }
    }

  }

}
