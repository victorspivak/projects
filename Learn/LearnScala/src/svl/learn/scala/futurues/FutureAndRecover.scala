package svl.learn.scala.futurues

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
object FutureAndRecover {
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

    def main(args: Array[String]) {
        test1()
        test2()
    }

    def test1 (){
        println("================================ test1 =======================================")
        val errorHandler: PartialFunction[scala.Throwable, Int] = {
            case e:Exception => 99999
        }

        val futures = Future.sequence(List(
            makeFuture(2, '2').recover(errorHandler),
            makeFuture(1, '1').recoverWith{
                case _ => makeFuture(2, '2')},
            makeFuture(3, '3').recover(errorHandler)))
        futures.map{
            values=>println(values)
        }

        println(Await result(futures, 10.seconds))
    }

    def test2 (){
        println("================================ test2 =======================================")
        val errorHandler: PartialFunction[scala.Throwable, Int] = {
            case e:Exception => 99999
        }

        val futures = Future.sequence(List(
            makeFuture(2, '2').recover(errorHandler),
            makeFuture(1, '1'). fallbackTo(makeFuture(2, '2')),
            makeFuture(3, '3').recover(errorHandler)))
        futures.map{
            values=>println(values)
        }

        println(Await result(futures, 10.seconds))
    }

    def makeFuture(value: Int, symbol: Char): Future[Int] = {
        Future(waiting(value, symbol))
    }
}
