package svl.learn.scala.implicits

object ImplicitClass extends App{
    object Helpers {
        implicit class IntWithTimes(x: Int) {
            def times[A](f: => A): Unit = {
                def loop(current: Int): Unit =
                    if(current > 0) {
                        f
                        loop(current - 1)
                    }
                loop(x)
            }

            def list () = List(x)
        }
    }


    import Helpers._
    5 times println("HI")

    println(5 list())
}
