package svl.learnspray

//import svl.spray.entities.Person

import akka.actor.Actor
import spray.routing._

import spray.json.DefaultJsonProtocol
import spray.httpx.unmarshalling._
import spray.httpx.marshalling._
import spray.http._
import HttpCharsets._
import MediaTypes._
import spray.json._

case class Person(name: String, firstName: String, age: Int)

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._
import spray.httpx.SprayJsonSupport._
import spray.util._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {

  val myRoute =
    path("") {
      get {
        respondWithMediaType(`text/html`) {
          // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            <html>
              <body>
                <h1>Say hello to
                  <i>spray-routing</i>
                  on
                  <i>spray-can</i>
                  !</h1>
              </body>
            </html> + "\n"
          }
        }
      }
    } ~
      path("test") {
        get {
          respondWithMediaType(`application/json`) {
            complete {
              implicit def executionContext = actorRefFactory.dispatcher
              val person = Person("Victor", "Spivak", 100)
              //person.toJson
              List(1, 2, 3).toJson
            }
          }
        }
      }
}
