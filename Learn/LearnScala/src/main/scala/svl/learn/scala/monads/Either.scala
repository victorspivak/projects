package svl.learn.scala.monads

object Either {
    def main(args: Array[String]) {
        val obj1 = Left("left")
        val obj2 = Right("right")
        val obj3 = Right(111)

        checker(obj1)
        checker(obj2)
        checker(obj3)

        def checker(obj:Either[_, _]) = obj match {
            case Left(s:String) => println(s)
            case Right(s:String) => println(s)
            case _ => println("Unknown")
        }

        println(obj1.right)

        val res1 = for{
            s <- obj1.right
        } yield s
        println(res1)

        val res2 = for{
            s <- obj2.right
        } yield s
        println(res2)
    }
}
