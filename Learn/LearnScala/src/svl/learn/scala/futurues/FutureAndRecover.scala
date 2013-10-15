package svl.learn.scala.futurues

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/*
 * User: victor    Date: 10/15/13   Time: 12:32 AM
 */
object FutureAndRecover {
    def main(args: Array[String]) {
        for (i <- 0 until 20) {
            test1(i, 5)
            test1(i, 0)
        }
    }

    def test1(m:Int, n: Int) = {
        val f = toFutureEither(getFutureInt(m, n))

        Await.result(f, 3 seconds).fold (
        {t =>
            println("Failure")
        },
        {r =>
            println("result=%d".format(r))
        }
        )
    }

    def toFutureEither(f: Future[Int]): Future[Either[Throwable, Int]]  =
        f map { Right(_) } recover { case x => Left(x) }


    def getFutureInt(m:Int, n: Int): Future[Int] = {
        val f = Future{
            Thread.sleep(2000)
            m / n
        }

        f.map(k => 2 * k).map(k => k + 5)
    }
}
