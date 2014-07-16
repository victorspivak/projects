package svl.learnspray

//import svl.learnspray.entities.Person
//
//import akka.actor.Actor
//import spray.routing._
//
//import spray.json.DefaultJsonProtocol
//import spray.httpx.unmarshalling._
//import spray.httpx.marshalling._
//import spray.http._
//import HttpCharsets._
//import MediaTypes._
//import spray.json._
//
////case class Person(name: String, firstName: String, age: Int)
//
//object MyJsonProtocol extends DefaultJsonProtocol {
//  implicit val PersonFormat = jsonFormat3(Person)
//}
//
//import MyJsonProtocol._





import akka.actor.Actor
import spray.routing._

import svl.learnspray.entities.Person

import spray.http.MediaTypes._
import spray.json._

import spray.json.DefaultJsonProtocol

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._

import spray.httpx.SprayJsonSupport._
import spray.util._


//val p = Person("Vic", "Spivak", 33)
//val personString = p.toJson.toString()
//val personJson = personString.parseJson
//val p2 = personJson.convertTo[Person]



class MyServiceActor extends Actor with MyService {
  def actorRefFactory = context
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
      path("person") {
        get {
          respondWithMediaType(`application/json`) {
            complete {
              implicit def executionContext = actorRefFactory.dispatcher
              val person = Person("Victor", "Spivak", 20)
              //also we could return: person
              person.toJson.toString()
            }
          }
        }
      } ~
//      path("test") {
//        post {
//          respondWithMediaType(`application/json`) {
//            complete {
//              implicit def executionContext = actorRefFactory.dispatcher
//              val person = Person("Victor", "Spivak", 20)
//              //also we could return: person
//              person.toJson.toString()
//            }
//          }
//        }
//      }
      path("person") {
        post {
          entity(as[Person]) { person =>
            println(person)
            val result = "OK"
            complete(result)
          }
        }
      }

}
