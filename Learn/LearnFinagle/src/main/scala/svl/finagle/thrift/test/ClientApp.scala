package svl.finagle.thrift.test

import org.apache.thrift.protocol.{TBinaryProtocol, TJSONProtocol}
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.thrift.ThriftClientFramedCodec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object ClientApp extends App {
  val myService = ClientBuilder()
    .hosts("localhost:" + Constants.MyTestServicePort)
    .codec(ThriftClientFramedCodec())
    .hostConnectionLimit(1)
    .build()

  val myClient = new MyTestService.ToScalaClient(myService)

  val response: Future[Seq[Byte]] = myClient.fetchBlob(7, 42)
  val result = Await.result(response, Duration("3 sec"))
  println("Got response!" + result.length)
  println(result.mkString(","))
  println("done")

  val response2: Future[String] = myClient.fetchString("abc", Some("xyz"))
  response2.onSuccess{case res => println("Got response - " + res)}
  println("after rpc sent")

  if (args.length > 0) {
    val fut =
      if (args(0) == "r")
        myClient.runSomething()
      else if (args(0) == "h")
        myClient.exceptHandled()
      else //if (args(0) == "u")
        myClient.exceptUnhandled()
    fut.onFailure {
      case ex: SomeException => println("!!!" + ex.message + " " + ex.value)
      case _ => println("???other exception")
    }
  }

  println("press any key to stop")
  System.in.read
  Await.ready(myClient.close(), Duration("1 sec"))
  println("end!")
}
