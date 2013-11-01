package svl.learn.scala.monads

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.Predef._
import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
/*
 * User: victor    Date: 10/12/13   Time: 5:46 PM
 */
object FutureAndTry {
    def waitingTry(timeout:Int, symbol:Char = '.') = {
        Try{
            waiting(timeout, symbol)
        }
    }


    def waiting(timeout: Int, symbol: Char): Int = {
        (1 to timeout) foreach {
            i =>
                print(symbol)
                Thread.sleep(1000)
        }
        println()

        if (timeout == 1) {
            println("************************************")
            throw new Exception("oops")
        }
        timeout
    }

    def sequence[T](xs : Seq[Try[T]]) : Try[Seq[T]] = {
        val rez = Try(xs.map {
            case Success(s) => s
            case Failure(e) => throw e
        })

        rez
    }

    def main(args: Array[String]) {
        test1()
        test2()
        test21()
        test3()
        test4()
    }

    def test1 (){
        try {
            println("================================ test1 =======================================")
            val futures = Future.sequence(List(Future(waiting(2, '2')), Future(waiting(1, '1')), Future(waiting(5, '5'))))

            futures.onComplete {
                case Success(s) =>
                    println(">>>>>>>>>>>>>>>>> 1")
                    println(s)
                case Failure(e) =>
                    println(">>>>>>>>>>>>>>>>> 2")
                    println(e.getMessage)
            }

            println(Await result(futures, 10.seconds))
        } catch {
            case e:Exception => println("==================> " + e)
        }
    }

    def test2 (){
        println("================================ test2 =======================================")
        val errorHandler: PartialFunction[scala.Throwable, Int] = {
            case e:Exception => 99999
        }

        val futures = Future.sequence(List( Future(waiting(2, '2')).recover(errorHandler),
                                            Future(waiting(1, '1')).recover(errorHandler),
                                            Future(waiting(5, '5')).recover(errorHandler)))
        futures.map{
            values=>println(values)
        }

        println(Await result(futures, 10.seconds))
    }

    def test21 (){
        println("================================ test21 =======================================")
        val errorHandler: PartialFunction[scala.Throwable, Int] = {
            case e:Exception => 99999
        }

        val futures = Future.sequence(List(
            makeFuture(2, '2').recover(errorHandler),
            makeFuture(1, '1').recover(errorHandler),
            makeFuture(3, '3').recover(errorHandler)))
        futures.map{
            values=>println(values)
        }

        println(Await result(futures, 10.seconds))
    }


    def makeFuture(value: Int, symbol: Char): Future[Int] = {
        Future(waiting(value, symbol))
    }

    def test3 (){
        println("================================ test3 =======================================")
        val futures = Future.sequence(List(Future(waitingTry(2, '2')), Future(waitingTry(1, '1')), Future(waitingTry(5, '5'))))

        futures.onComplete {
            case Success(s) =>
                println(">>>>>>>>>>>>>>>>> 1")
                    sequence(s) match {
                        case Success(rez) => println(rez)
                        case Failure(e) =>
                            println(">>>>>>>>>>>>>>>>> 3")
                            println(e.getMessage)
                    }
            case Failure(e) =>
                println(">>>>>>>>>>>>>>>>> 2")
                println(e.getMessage)
        }

        println(Await result(futures, 10.seconds))
    }

    def test4 (){
        println("================================ test4 =======================================")
        val futures = Future.sequence(List(Future(waitingTry(2, '2')), Future(waitingTry(1, '1')), Future(waitingTry(5, '5'))))

        futures.map{values=>
            sequence(values) match{
                case Success(rez) => println(rez)
                case Failure(e) =>
                    println(">>>>>>>>>>>>>>>>> 3")
                    println(e.getMessage)
            }
        }
        println(Await result(futures, 10.seconds))
    }
}
