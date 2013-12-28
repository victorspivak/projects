package svl.learn.scala.akka.distributed

import akka.util.Timeout
import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import akka.io.Tcp
import scala.language.postfixOps

import akka.actor.{ Props, Deploy, Address, AddressFromURIString }
import akka.remote.RemoteScope
import com.typesafe.config.ConfigFactory

object GreeterClient {
  implicit val timeout = Timeout(1 second)

  def main(args: Array[String]) {
    val system = ActorSystem("HelloSystem", ConfigFactory.load(ConfigFactory.parseString("""
  akka {
    remote.netty.tcp.port = 2553
    loglevel = "DEBUG"
    stdout-loglevel = "DEBUG"
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
    val actor = system.actorOf(Props[ClientActor], name = "client_actor")


    actor ! "start"

    Thread.sleep(10000)
    system.stop(actor)
    system.shutdown()
    system.awaitTermination()
  }

  class ClientActor extends Actor{
    val actor = context.system.actorSelection("akka.tcp://HelloSystem@127.0.0.1:2552/user/myactor")
    def receive = {
      case "start" =>
        actor ! "Hi"
        actor ! "Oops"
        actor ! "Hello"
//    (actor ? "wait").onSuccess{
//      case msg => println(s"Got $msg")
//    }
//    actor ! "quit"

      case msg:String => println(msg)
    }
  }
}
