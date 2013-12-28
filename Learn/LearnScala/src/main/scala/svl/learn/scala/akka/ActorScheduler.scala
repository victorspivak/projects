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

object ActorScheduler {
   implicit val timeout = Timeout(1 second)

   def main(args: Array[String]) {
     val system = ActorSystem("HelloSystem")
     val actor = system.actorOf(Props[MyActor], name = "myActor")

     implicit val receiver = system.actorOf(Props[MyReceiver], name = "myReceiver")

     actor ! "greet"
     actor ! "greet"
     actor ! "greet"
     actor ! "greet"
     actor ! "greet"

     Thread.sleep(3000)

     system.shutdown()
     system.awaitTermination()
   }

   class MyActor extends Actor{
     context.system.scheduler.schedule(100 millis, 100 millis, context.self, "timer")

     def receive = {
       case "greet" =>
         context.system.eventStream.publish("Hello from: " + self)
         sender ! "Hello from: " + self
       case "timer" =>
         println("Timer")
       case "exit" => context.stop(self)
     }
   }

   class MyReceiver extends Actor{
     def receive = {
       case msg:String => println(">>>>>>>>>>>>>>>>>>> " + msg)
     }
   }
 }
