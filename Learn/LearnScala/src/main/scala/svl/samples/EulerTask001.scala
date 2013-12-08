import java.io.PrintStream

object EulerTask001 {
    def main(args:Array[String]) {
        val answer = (1 to  999).filter( n => (n % 3) == 0 || (n % 5) == 0).sum

        println("Euler001: " + answer)

        def fib (n:Int) = {
            def fib (list:List[Int], a:Int, b:Int) :List[Int] = {
                val newFib = a + b
                if (newFib < n)
                    fib(newFib :: list, b, newFib)
                else
                    list.reverse
            }

            fib(List(), 0, 1)
        }
        println(fib(100))
        println("Euler002: " + fib(4000000).filter(_%2 == 0).sum)

        def isPrime(list: scala.List[Long], candidate: Long): Boolean = {
            !list.reverse.exists(candidate % _ == 0)
        }
        def primes (n:Long) = {
            def primes(list:List[Long], candidate:Long) :List[Long] = {
                if (candidate >= n)
                    list.reverse
                else if(isPrime(list, candidate))
                    primes(candidate :: list, candidate + 2)
                else
                    primes(list, candidate + 2)
            }

            primes (List(2), 3)
        }

        def euler003 (n:Long) = {
            println(primes((n + 1)/2).reverse)
            primes((n + 1)/2).reverse.find(n % _ == 0) match {
                case None => 1
                case Some(n) => n
            }
        }

        println(primes(100))

        def withTracing(out: PrintStream)(op :  => Unit) {
            val start = System.currentTimeMillis()
            op
            val time = System.currentTimeMillis() - start
            out.println ("Tracing Time: " + time)
        }
        withTracing(System.out) {
            primes(100000)
        }

        //println("Euler003: " + euler003(600851475143L))
    }
}

