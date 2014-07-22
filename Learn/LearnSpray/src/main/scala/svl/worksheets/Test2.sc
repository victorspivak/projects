import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask
import akka.io.IO
import spray.can.Http
import spray.http._
import HttpMethods._

implicit val system: ActorSystem = ActorSystem()
implicit val timeout: Timeout = Timeout(15.seconds)
import system.dispatcher // implicit execution context

val response: Future[HttpResponse] =
  (IO(Http) ? HttpRequest(GET, Uri("http://localhost:8080/user"))).mapTo[HttpResponse]

// or, with making use of spray-httpx
import spray.httpx.RequestBuilding._

val response2: Future[HttpResponse] =
  (IO(Http) ? Get("http://spray.io")).mapTo[HttpResponse]





