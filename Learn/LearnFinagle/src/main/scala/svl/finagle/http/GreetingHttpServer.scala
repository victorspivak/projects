package svl.finagle.http

import java.util.concurrent.{Executors, ExecutorService, TimeUnit}

import com.twitter.finagle.http.path.{Root, Path}
import com.twitter.finagle.http.path._
import com.twitter.finagle.{SimpleFilter, Http, Service}
import com.twitter.util._
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpMethod._
import org.jboss.netty.util.CharsetUtil
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.NoTypeHints
import org.json4s.native.Serialization
import org.jboss.netty.handler.codec.http.HttpVersion.HTTP_1_1

import scala.collection.mutable
import scala.util.Random

case class Person(firstName:String, lastName:String, email:String)

object PersonService {
  val persons = mutable.Map[String, Person](
    "vic" -> Person("Vic", "Spivak", "vic@box.com"),
    "test" -> Person("Test", "Test", "test@test.com")
  )

  val random = new Random()
  def personById(id:String) = persons get id
  def update(id:String, person:Person) = persons(id) = person
  def add(id:String, person:Person) = persons(id) = person
  def all = {
    Thread.sleep(random.nextInt(2000))
    persons.values
  }
}

object Greeting {
  def greet(person:Person) = s"Hello ${person.firstName} ${person.lastName}"
}

object GreetingHttpServer extends App {
  implicit val formats = Serialization.formats(NoTypeHints)
  val pool: ExecutorService = Executors.newFixedThreadPool(2)

  class HandleExceptions extends SimpleFilter[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
      service(request) handle { case error =>
        val statusCode = error match {
          case _: IllegalArgumentException => HttpResponseStatus.FORBIDDEN
          case _: TimeoutException => HttpResponseStatus.GATEWAY_TIMEOUT
          case _ => HttpResponseStatus.INTERNAL_SERVER_ERROR
        }
        val errorResponse = new DefaultHttpResponse(HTTP_1_1, statusCode)
        val message: String = s"${error.getClass.getName}  ${error.getMessage}\n"
        errorResponse.setContent(ChannelBuffers.copiedBuffer(message, CharsetUtil.UTF_8))
        errorResponse
      }
    }
  }

  class Authorize extends SimpleFilter[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest, continue: Service[HttpRequest, HttpResponse]) = {
      if ("secret" == request.headers().get(HttpHeaders.Names.AUTHORIZATION)) {
        continue(request)
      } else {
        Future.exception(new IllegalArgumentException("You don't know the secret"))
      }
    }
  }

  class TimeoutFilter(timeout: Duration, timer: Timer) extends SimpleFilter[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]): Future[HttpResponse] = {
      val res = service(request)
      res.within(timer, timeout)
    }
  }

  class Respond extends Service[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest) = {
      val PersonPath = "persons"
      val GreetingPath = "greeting"
      request.getMethod -> Path(request.getUri) match {
        case GET -> Root/PersonPath => scheduleTask(FutureTask({
          val all = PersonService.all.toList
          makeResponseWithStringBody(request, Serialization.write(all) + "\n")
        }))
        case GET -> Root/PersonPath/personId => scheduleTask(FutureTask({
          makeResponseWithStringBody(request, PersonService.personById(personId).map(p => Serialization.write(p) + "\n"))
        }))
        case PUT -> Root/PersonPath/personId => scheduleTask(FutureTask({
          PersonService.personById(personId) match {
            case Some(p) =>
              PersonService.update(personId, parse(request.getContent.toString(CharsetUtil.UTF_8)).extract[Person])
              new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK)
            case None => new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.NOT_FOUND)
          }
        }))
        case POST -> Root/PersonPath/personId => scheduleTask(FutureTask({
          PersonService.personById(personId) match {
            case Some(p) => new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.NOT_ACCEPTABLE)
            case None =>
              PersonService.add(personId, parse(request.getContent.toString(CharsetUtil.UTF_8)).extract[Person])
              new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK)
          }
        }))
        case GET -> Root/GreetingPath/personId => scheduleTask(FutureTask({
          val body = PersonService.personById(personId).map(p => Greeting.greet(p) + "\n")
          makeResponseWithStringBody(request, body)
        }))
        case _ =>
          Future.value(new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK))
      }
    }
  }

  def scheduleTask(task: FutureTask[HttpResponse]): FutureTask[HttpResponse] = {
    pool.submit(task)
    task
  }

  val handleExceptions = new HandleExceptions
  val authorize = new Authorize
  val respond = new Respond
  private val timer: JavaTimer = new JavaTimer()
  val timeout = new TimeoutFilter(Duration(1000, TimeUnit.MILLISECONDS), timer)
  val service: Service[HttpRequest, HttpResponse] = handleExceptions andThen authorize andThen timeout andThen respond

  val server = Http.serve(":8080", service)
  Await.ready(server)

  def makeResponseWithStringBody(request: HttpRequest, body: Option[String]): DefaultHttpResponse = body match {
    case Some(text) => makeResponseWithStringBody(request, text + "\n")
    case None => new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.NOT_FOUND)
  }

  def makeResponseWithStringBody(request: HttpRequest, body: String, httpCode: HttpResponseStatus = HttpResponseStatus.OK): DefaultHttpResponse = {
    val response = new DefaultHttpResponse(request.getProtocolVersion, httpCode)
    response.setContent(ChannelBuffers.copiedBuffer(body, CharsetUtil.UTF_8))
    response
  }
}
