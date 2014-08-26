//package svl.finagle.thrift
//
//import com.twitter.finagle.Thrift
//import com.twitter.util.{Await, Future}
//import svl.finagle.thrift.thriftscala.Greeting
//
//object ThriftServer {
//  def main(args: Array[String]) {
//    //#thriftserverapi
//    val server = Thrift.serveIface("localhost:8080", new Greeting[Future] {
//      def hi() = Future.value("hi")
//    })
//    Await.ready(server)
//    //#thriftserverapi
//  }
//}
