package svl.finagle.http

import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Await, Future}
import org.jboss.netty.handler.codec.http._

object GreetingHttpClient extends App {
  val client: Service[HttpRequest, HttpResponse] =
    Http.newService("www.google.com:80")
  val request =  new DefaultHttpRequest(
    HttpVersion.HTTP_1_1, HttpMethod.GET, "/")
  val response: Future[HttpResponse] = client(request)
  response onSuccess { resp: HttpResponse =>
    println("GET success: " + resp)
  }

  response onFailure (e => println(e))

  Await.ready(response)
  Thread.sleep(1000)
}
