package svl.learnspray

import akka.actor.Actor
import spray.routing._

import svl.learnspray.entities.{User, MyJsonProtocol, Person}

import spray.http.MediaTypes._
import spray.json._

import MyJsonProtocol._

import spray.httpx.SprayJsonSupport._
import spray.util._

class MyServiceActor extends Actor with MyService {
  def actorRefFactory = context
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {
  var currentUser = User("Vic", "Spivak", Some("victor.spivak@gmail.com"), Some("Box"))

  val myRoute =
    path("") {
      get {
        respondWithMediaType(`text/html`) {
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
      path("person") {
        post {
          entity(as[Person]) { person =>
            println(person)
            val result = "OK"
            complete(result)
          }
        }
      }~
      path("user") {
        get {
          respondWithMediaType(`application/json`) {
            complete {
              implicit def executionContext = actorRefFactory.dispatcher
              currentUser.toJson.toString()
            }
          }
        }
      } ~
        path("user") {
          post {
            entity(as[User]) { user =>
              println(user)
              currentUser = user
              val result = "OK"
              complete(result)
            }
          }
        }

}
