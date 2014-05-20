package svl.learn.scala.classes

object ImplicitClasses {
  def main(args: Array[String]) {
    implicit class IntWithTimes(x:Int) {
      def times[A](f: => A): Unit  = {
        def loop(current:Int) : Unit =
          if (current > 0) {
            f
            loop(current - 1)
          }

        loop(x)
      }
    }

    5 times println("Hello")
  }
}
