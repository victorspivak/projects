package svl.learn.scala.monads

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.Predef._
import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

/*
 * User: victor    Date: 10/15/13   Time: 8:06 PM
 */
object FutureErrorHandling {
    def func(value:Int): Int = {
        if (value == 0) {
            println("************************************")
            throw new Exception("oops")
        }
        value
    }

    def main(args: Array[String]) {
        test1()
    }

    def test1 (){
        val f1 = future{func(0)}
        val f1_1 = future{func(1)}
        val f2 = f1.map{_+10}
        val f3 = f2.map{v =>v + func(100)}
        val f4 = f1_1.map{_+1000}

        val f: Future[List[Int]] = Future.sequence(List(f3, f4))

//        val result1 = f.recover{
//            case e:Exception => List(9999)
//            case _ => List(1000)
//        }
//
//        println("Before")
//        val result = Await result (result1, 10.second)
//        println("After")

//        f.onComplete{
//            case Success(v) => println(v)
//            case Failure(e) => println(e)
//        }

//        val resultAsFuture = Await.ready(f, 10.second)
//        resultAsFuture.map{r => println("=======>" + r)}
//        println("???????> " + resultAsFuture.failed)

        val resultAsFuture = Await.ready(f, 10.second)
        val value = resultAsFuture.value
        println(value)


        println("Before")
//        val result = Await result (f, 10.second)
//        println("After")
//
//        println(result)
    }
}
