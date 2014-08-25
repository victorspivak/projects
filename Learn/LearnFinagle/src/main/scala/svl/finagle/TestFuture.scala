package svl.finagle

import java.util.concurrent.{Executors, ExecutorService}

import com.twitter.util.{Future,FutureTask}


object TestFuture extends App {
  val pool: ExecutorService = Executors.newFixedThreadPool(2)

  test2()

  def test1() {
    val f1: Future[Unit] = Future ({
      println("Start")
      Thread.sleep(5000)
      println("Stop")
    })
    println("After Construction 1 : " + f1)
  }

  def test2() {
    val f1 = new FutureTask[Unit]({
      println("Start FutureTask")
      Thread.sleep(10000)
      println("Stop FutureTask")
    })
    println("After Construction 2")
    pool.execute(f1)
    println("After Submission")
  }

  def test3(): Unit = {

    println("Start")
    val ft = new FutureTask[Int]( { Thread.sleep(5000); 10 } )
    println("After task creation")
    pool.execute(ft)
    println("After submission")
    println(ft.get())
  }
}
