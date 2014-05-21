package svl.learn.scala.monads

import scala.util.{Failure, Success, Try}
import scala.Predef._
import scala.concurrent.duration._
import scala.concurrent._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

object FutureAndTryModified {
  def func(value: Int): Int = {
    Thread.sleep(1000)
    if (value == 0) {
      println("************************************")
      throw new Exception("oops")
    }
    Thread.sleep(1000)
    value
  }

  def main(args: Array[String]) {
    test1()
    //		test2()
  }

  def test1() {
    implicit def tryToFuture[T](t: Try[T]): Future[T] = {
      t match {
        case Success(s) => Future.successful(s)
        case Failure(ex) => Future.failed(ex)
      }
    }

    val f1 = future {
      Try {
        println("===> f1 started")
        func(0)
      }
    }
    val f1_1 = future {
      Try {
        println("===> f2 started")
        func(1)
      }
    }
    val f2 = f1.flatMap {
      _.map {
        _ + 10
      }
    }

    val f3 = f2.flatMap { v =>
      Try(v + func(100))
    }
    val f4 = f1_1.flatMap {
      _.map(_ + 1000)
    }

    val f: Future[List[Int]] = Future.sequence(List(f3, f4))

    println("Before...")
    val result = Await result(f, 10.second)
    println("After...")

    println(result)
  }

  def test2() {
    val f1 = future {
      Try(func(0))
    }
    val f1_1 = future {
      Try(func(1))
    }
    val f2 = f1.map {
      _.map {
        _ + 10
      }
    }

    val f3 = f2.map {
      _.flatMap {
        v =>
          Try(v + func(100))
      }
    }
    val f4 = f1_1.map {
      _.map(_ + 1000)
    }

    val f: Future[List[Try[Int]]] = Future.sequence(List(f3, f4))

    val result = Await result(f, 10.second)

    println(result)
  }
}
