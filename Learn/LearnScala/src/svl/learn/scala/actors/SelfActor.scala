import scala.actors.Actor._

self ! "hello"
self.receive { case x => println(x) }


