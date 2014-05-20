package svl.learn.scala.classes

object TypeClasses {
  def main(args: Array[String]) {
    trait Show[A] {
      def shows(a:A): String
    }

    def shows[A](a:A)(implicit shower:Show[A]) = shower.shows(a)

    //In the unaryShows we define one implicit and later on we define another one and they coexist
    def unaryShows(x:Int) : String = {
      implicit val AnotherIntShow = new Show[Int] {
        override def shows(a: Int) = ((1 to a) map { i: Int => "|"}).mkString
      }

      shows(x)
    }

    implicit val IntShow = new Show[Int] {
      override def shows(a: Int) = "|%d|".format(a)
    }

    println(shows(10))
    println(shows(20))

    //alternative syntax
    def showsA[A:Show](a:A) = implicitly[Show[A]].shows(a)

    println(showsA(10))
    println(showsA(20))


    println(unaryShows(10))
    println(unaryShows(20))
  }
}
