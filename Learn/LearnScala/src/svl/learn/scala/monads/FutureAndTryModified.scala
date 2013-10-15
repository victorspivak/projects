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
object FutureAndTryModified {
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
        val f1 = future{Try(func(0))}
        val f1_1 = future{Try(func(1))}
        val f2 = f1.map{_.map{
            _+10
        }}

        val f3 = f2.map{
            _.flatMap{v =>
                Try(v + func(100))}
        }
        val f4 = f1_1.map{_.map(_+1000)}

        val result = Await result (Future.sequence(List(f3, f4)), 10.second)

        println(result)
    }
}
