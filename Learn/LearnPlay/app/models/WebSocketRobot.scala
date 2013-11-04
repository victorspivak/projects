package models

import akka.actor._
import scala.concurrent.duration._
import scala.language.postfixOps

import play.api.libs.json._
import play.api.libs.iteratee._
import play.api.libs.concurrent._

import akka.util.Timeout
import akka.pattern.ask

import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

object WebSocketRobot {
  implicit val timeout = Timeout(1 second)

  lazy val default = {
    Akka.system.actorOf(Props[WebSocketRobot])
  }
  val loggerIteratee = Iteratee.foreach[JsValue](event => println(s"Trace: ${event.toString()}"))

  def start():scala.concurrent.Future[(Iteratee[JsValue,_],Enumerator[JsValue])] = {
    (default ? Connect()).map {
      case enumerator:Enumerator[JsValue] =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          default ! Command((event \ "command").as[String])
        }
        enumerator |>> loggerIteratee

        (iteratee,enumerator)
    }
  }
}

class WebSocketRobot extends Actor {
  val (myEnumerator, myChannel) = Concurrent.broadcast[JsValue]

  def receive = {
    case Connect() => sender ! myEnumerator
    case Command(input) =>
      val msg = JsObject(
        Seq(
          "text" -> JsString(input.reverse)
          )
      )
      myChannel.push(msg)
  }
}

case class Connect()
case class Command(command:String)
