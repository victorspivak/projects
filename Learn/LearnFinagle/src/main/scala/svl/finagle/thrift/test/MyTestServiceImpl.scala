package svl.finagle.thrift.test

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

class MyTestServiceImpl extends MyTestService.ScalaFutureIface {
  override def fetchBlob(id: Long, id2: Int): Future[Seq[Byte]] = {
    val buff = List[Byte](id.toByte, id2.toByte, 2, 3, 4, 5, 99)
    Future(buff)
  }

  def asyncFetchString(id: String, msg: Option[String]): String = {
    java.lang.Thread.sleep(1000)
    "test: " + id + " " + msg.get
  }

  override def fetchString(id: String, msg: Option[String]): Future[String] = {
    //Future.value("test: " + id + " " + msg.get)
    future { asyncFetchString(id, msg) }
  }

  override def runSomething(): Future[Unit] = {
    // Do something here
    println("Running....")
    Future.successful()
  }

  def exceptUnhandled(): Future[Unit]  = {
    throw SomeException("unhandled exception on server", 1)
  }

  def exceptHandled(): Future[Unit] = {
    throw SomeException("handled exception on server", 2)
  }

  override def close(): Future[Unit] = {
    // Overriding this method is optional
    Future.successful()
  }
}
