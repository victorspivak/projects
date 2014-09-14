package svl.finagle.thrift.test

import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.thrift.ThriftServerFramedCodec
import java.net.InetSocketAddress
import org.apache.thrift.protocol.TBinaryProtocol
import scala.concurrent.ExecutionContext.Implicits.global

object ServerApp extends App {
  val myserviceImpl = new MyTestServiceImpl

  val myServerBuilder = ServerBuilder()
    .name("MyTestServer")
    .bindTo(new InetSocketAddress(Constants.MyTestServicePort))
    .codec(ThriftServerFramedCodec())
    .maxConcurrentRequests(5)

  val myServer = new MyTestService.ToScalaServer(myServerBuilder, myserviceImpl)

  println("press any key to stop")
  System.in.read
  println("shutting down...")
  myServer.close()
  myserviceImpl.close()
  println("end!")
}
