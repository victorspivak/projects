package svl.learn.scala.akka.distributed

import akka.util.Timeout
import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import akka.io.Tcp
import scala.language.postfixOps
import com.typesafe.config.ConfigFactory

object GreeterServer {
  implicit val timeout = Timeout(1 second)

  def main(args: Array[String]) {
    val system = ActorSystem("HelloSystem", ConfigFactory.load(ConfigFactory.parseString("""
  akka {
    loglevel = "DEBUG"
    stdout-loglevel = "DEBUG"
    remote.netty.tcp.port = 2552
    actor {
      provider = "akka.remote.RemoteActorRefProvider"
    }
    remote {
      netty.tcp {
        hostname = "127.0.0.1"
      }
    }
 }
""")))

    val actor = system.actorOf(Props[MyActor], name = "myactor")

    actor ! "Hi"

    val path = actor.path
    println(s"Actor path: $path address: ${path.address}")
  }

  class MyActor extends Actor{
    def receive = {
      case "quit" => context.stop(self)
        context.system.shutdown()
        context.system.awaitTermination()
      case "Hi" => println("Hello Client")
      case "Hello" => sender ! "Hello sir"
      case "wait" => Thread.sleep(1000)
        sender ! "done"
      case msg:String => println(msg)
    }
  }
}
