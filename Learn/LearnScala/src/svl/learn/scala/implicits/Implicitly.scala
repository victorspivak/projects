package svl.learn.scala.implicits

object Implicitly extends App{
    implicit val luckyNumber = 5

    def guessLuckyNumber() = {
        implicitly[Int]
    }

    println (guessLuckyNumber())
}
