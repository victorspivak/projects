package svl.finagle.http

import com.twitter.finagle.http.path.{Root, Path}
import com.twitter.finagle.http.path._
import com.twitter.finagle.{Http, Service}
import com.twitter.util.{Await, Future}
import org.jboss.netty.buffer.ChannelBuffers
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpMethod._
import org.jboss.netty.util.CharsetUtil
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

import scala.collection.mutable

case class Person(firstName:String, lastName:String, email:String)

object PersonService {
  val persons = mutable.Map[String, Person](
    "vic" -> Person("Vic", "Spivak", "vic@box.com"),
    "test" -> Person("Test", "Test", "test@test.com")
  )

  def personById(id:String) = persons get id
  def update(id:String, person:Person) = persons(id) = person
  def add(id:String, person:Person) = persons(id) = person
  def all = persons.values
}

object Greeting {
  def greet(person:Person) = s"Hello ${person.firstName} ${person.lastName}"
}

object GreetingHttpServer extends App {
  implicit val formats = Serialization.formats(NoTypeHints)

  val service = new Service[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest): Future[HttpResponse] = {
      try{
        request.getMethod -> Path(request.getUri) match {
          case GET -> Root / "person" => Future.value {
            val all = PersonService.all.toList
            makeResponseWithStringBody(request, Serialization.write(all) + "\n")
          }
          case GET -> Root / "person" / personId => Future.value (
            makeResponseWithStringBody(request, PersonService.personById(personId).map(p => Serialization.write(p) + "\n"))
          )
          case GET -> Root / "greeting" / personId => Future.value {
            val body = PersonService.personById(personId).map(p => Greeting.greet(p) + "\n")
            makeResponseWithStringBody(request, body)
          }
          case PUT -> Root / "person" / personId => Future.value (PersonService.personById(personId) match {
            case Some(p) =>
              PersonService.update(personId, parse(request.getContent.toString(CharsetUtil.UTF_8)).extract[Person])
              new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK)
            case None => new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.NOT_FOUND)
          })
          case POST -> Root / "person" / personId => Future.value (PersonService.personById(personId) match {
            case Some(p) => new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.NOT_ACCEPTABLE)
            case None =>
              PersonService.add(personId, parse(request.getContent.toString(CharsetUtil.UTF_8)).extract[Person])
              new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK)
          })
          case _ =>
            Future.value(new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.OK))
        }
      } catch {
        case e: Exception => Future.value {
          val message = Option(e.getMessage) getOrElse "Something went wrong."
          makeResponseWithStringBody(request, message, HttpResponseStatus.INTERNAL_SERVER_ERROR)
        }
      }
    }
  }

  def makeResponseWithStringBody(request: HttpRequest, body: Option[String]): DefaultHttpResponse = body match {
    case Some(text) => makeResponseWithStringBody(request, text + "\n")
    case None => new DefaultHttpResponse(request.getProtocolVersion, HttpResponseStatus.NOT_FOUND)
  }

  def makeResponseWithStringBody(request: HttpRequest, body: String, httpCode: HttpResponseStatus = HttpResponseStatus.OK): DefaultHttpResponse = {
    val response = new DefaultHttpResponse(request.getProtocolVersion, httpCode)
    response.setContent(ChannelBuffers.copiedBuffer(body, CharsetUtil.UTF_8))
    response
  }

  val server = Http.serve(":8080", service)
  Await.ready(server)
}
