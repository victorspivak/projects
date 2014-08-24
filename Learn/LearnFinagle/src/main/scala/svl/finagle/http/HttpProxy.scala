package svl.finagle.http

import com.twitter.finagle.{Http, Service}
import com.twitter.util.Await
import org.jboss.netty.handler.codec.http._

object HttpProxy extends App {
  val client: Service[HttpRequest, HttpResponse] =
//    Http.newService("www.google.com:80")
    Http.newService("www.lenta.ru:80")

  val server = Http.serve(":8080", client)
  Await.ready(server)
}

