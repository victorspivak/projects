import svl.scala.tools.Performance._;

object EulerSolutions {
    def main(args:Array[String]) {
        //val r = (1 until 1000).view.filter(n => n % 3 == 0 || n % 5 == 0).sum
        //
        //assert(r == 233168) // 7 ms


        //println(ps.view.take(100).foreach(println))


        def euler003 (n:Long) = {
            lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
                ps.takeWhile(j => j * j <= i).forall(i % _ > 0))

            def findFactor (n:Long) : Long = {
                val limit = math.sqrt(n)
                ps.view.takeWhile(_ < limit).find(n % _ == 0) match {
                    case Some(factor) => findFactor(n / factor)
                    case None => n
                }
            }

            findFactor(n)
        }

        withTiming(System.out) {
            println("600851475143L: " + euler003(600851475143L))
        }
        withTiming(System.out) {
            println("600851475143L: " + euler003(600851475143L))
        }

        withTiming {
            println("600851475143L: " + euler003(600851475143L))
        }

        println("34: " + euler003(34))
        println("17: " + euler003(17))

        println("600851475143L: " + euler003(600851475143L))
    }
}

