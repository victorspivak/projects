import actors.Actor
import scala.actors.Actor._

object SillyActor extends Actor {
    def act() {
        for (i <- 1 to 5) {
            println("I'm acting!")
            Thread.sleep(1000)
        }
    }
}

object SeriousActor extends Actor {
    def act() {
        for (i <- 1 to 5) {
            println("To be or not to be.")
            Thread.sleep(1000)
        }
    }
}

SillyActor.start(); SeriousActor.start()

val seriousActor2 = actor {
    for (i <- 1 to 5)
        println("That is the question.")
    Thread.sleep(1000)
}

