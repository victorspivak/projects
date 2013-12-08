package svl.learn.scala.futurues

import scala.concurrent.{Await, Future}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object Futures {
    def main(args: Array[String]) {
        func1()
        println("===============================================")
        func2()
        println("===============================================")
        func3()
        println("===============================================")
        func4()

        def func1() = {
            val f1 = future{
                println("f1 is started")
                Thread.sleep(5000)
                println("f1 is finished")
                1
            }

            println("f2 is constructing...")
            val f2 = for {
                v <- f1
                l1 = println("f2 is started")
                delay = Thread.sleep(3000)
                l2 = println("f2 is finished")
            } yield v + 10

            println("f3 is constructing...")
            val f3 = for {
                v <- f2
                l1 = println("f3 is started")
                delay = Thread.sleep(1000)
                l2 = println("f3 is finished")
            } yield v + 100

            println("f4 is constructing...")
            val f4 = for {
                v <- f2
                l1 = println("f4 is started")
                delay = Thread.sleep(1500)
                l2 = println("f4 is finished")
            } yield v + 1000

            val result = Await result (Future.sequence(List(f3, f4)), 10.second)

            println(result)
        }

        def func2() = {
            val f1 = future{
                println("f1 is started")
                Thread.sleep(5000)
                println("f1 is finished")
                1
            }

            val f2 = f1 map {v =>
                println("f2 is started")
                Thread.sleep(3000)
                println("f2 is finished")
                v + 10
            }

            val f3 = f2 map {v =>
                println("f3 is started")
                Thread.sleep(1000)
                println("f3 is finished")
                v + 100
            }

            val result = Await result (f3, 10.second)

            println(result)
        }

        def func3() = {
            val f1 = Future{
                println("f1 is started")
                Thread.sleep(5000)
                println("f1 is finished")
                1
            }

            val f2 = Future{
                println("f2 is started")
                Thread.sleep(3000)
                println("f2 is finished")
                val rez = (Await result (f1, 10.second)) + 10
                println("f2 finished calculation")
                rez
            }

            val f3 = Future{
                println("f3 is started")
                Thread.sleep(1000)
                println("f3 is finished")
                val rez = (Await result (f1, 10.second)) + 100

                println("f3 finished calculation")

                rez
            }

            val fs = Future.sequence(List(f2, f3))
            val result = Await result (fs, 10.second)

            println(result)
        }

        def func4() = {
            val f1 = Future{
                println("f1 is started")
                Thread.sleep(5000)
                println("f1 is finished")
                1
            }

            val f2 = Future{
                println("f2 is started")
                Thread.sleep(3000)
                println("f2 is finished")
                10
            }

            val f3 = Future{
                println("f3 is started")
                Thread.sleep(1000)
                println("f3 is finished")
                100
            }

            val rez = for {
                x <- f1
                y <- f2
                z <- f3
            } yield x + y + z

            println("Start final waiting...")
            val result = Await result (rez, 10.second)

            println(result)
        }
    }
}
