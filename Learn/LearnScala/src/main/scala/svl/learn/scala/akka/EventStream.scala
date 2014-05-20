package svl.learn.scala.akka

import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Restart
import scala.language.postfixOps

object EventStream {
  implicit val timeout = Timeout(1 second)

  def main(args: Array[String]) {
    val system = ActorSystem("HelloSystem")
    val actor = system.actorOf(Props[MyActor], name = "myActor")
    val listener1 = system.actorOf(Props(classOf[MyListener], 1), name = "myListener1")
    val listener2 = system.actorOf(Props(classOf[MyListener], 2), name = "myListener2")

    implicit val receiver = system.actorOf(Props[MyReceiver], name = "myReceiver")


    actor ! "greet"
    actor ! "greet"
    actor ! "greet"
    actor ! "greet"

    Thread.sleep(1000)
    system.eventStream.subscribe(listener1, classOf[Any])

    system.eventStream.subscribe(listener2, classOf[Any])
    actor ! "greet"

    Thread.sleep(3000)

    system.shutdown()
    system.awaitTermination()
  }

  class MyActor extends Actor {
    def receive = {
      case "greet" =>
        context.system.eventStream.publish("Hello from: " + self)
        sender ! "Hello from: " + self
      case "exit" => context.stop(self)
    }
  }

  class MyReceiver extends Actor {
    def receive = {
      case msg: String => println(">>>>>>>>>>>>>>>>>>> " + msg)
    }
  }

  class MyListener(val id: Int) extends Actor {
    def receive = {
      case event => println(s"*********************> $id => $event")
    }
  }

}
