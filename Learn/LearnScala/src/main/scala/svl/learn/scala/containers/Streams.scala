import Stream._
import scala.language.postfixOps

object Streams {
    def main(args:Array[String]) {
        def from(n: Int): Stream[Int] =
            Stream.cons(n, from(n + 1))

        def sieve(s: Stream[Int]): Stream[Int] =
            Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))

        def primes = sieve(from(2))

        primes take 100 print

        Console.println()

        1 #:: 2 #:: empty foreach (println _)

        def i(x:Int,y:Int):Stream[Int] = (x*y) #:: i(x+1,y*2)
        i(2,3) take 3 foreach println


        Console.println("Fib")
        def fib(x:Int,y:Int):Stream[Int] = y #:: fib(y,x + y)
        fib(1, 1) take 10 foreach println
    }
}

