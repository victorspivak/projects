import scala.actors.Actor._

object SelfActor {
    def main(args:Array[String]) {
        self ! "hello"
        self.receive { case x => println(x) }
    }
}


