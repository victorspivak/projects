package lib

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
import org.slf4j.LoggerFactory
import controllers.BoxContext
import scala.concurrent.Future
import model.BoxItem._
import model.BoxFolderItems

object AutoCompleteService {
  implicit val timeout = Timeout(1 second)
  val logger = LoggerFactory.getLogger("AutoCompleteService")

  val loggerIteratee = Iteratee.foreach[JsValue](event => logger.debug(s"${event.toString()}"))

  def start(boxContext:BoxContext) = {
    val props = Props(classOf[AutoCompleter], boxContext)
    val autoCompleter = Akka.system.actorOf(props)
    (autoCompleter ? Connect()).map {
      case enumerator:Enumerator[JsValue @unchecked] =>
        val iteratee = Iteratee.foreach[JsValue] { event =>
          (event \ "command").as[String] match{
            case "close_ac_actor" =>
              logger.debug("Close actor")
              autoCompleter ! PoisonPill
            case command:String => autoCompleter ! Command(command)
          }
        }
        enumerator |>> loggerIteratee

        (iteratee,enumerator)
    }
  }
}

class AutoCompleter(boxContext:BoxContext) extends Actor {
  val (myEnumerator, myChannel) = Concurrent.broadcast[JsValue]

  def receive = getBehavior(None)
  def getBehavior(folderItems:Option[BoxFolderItems]): Receive = {
    case Connect() => sender ! myEnumerator
    case Command(input) =>
      //todo svl 04 Nov 13 (8:15 PM) vspivak: Fix the following hacky code
      if (input.startsWith("cd")) {
        val params = input.substring(2).trim

        folderItems match {
          case Some(boxFolderItems) => sendSuggestion(boxFolderItems, params)
          case None =>
            boxContext.boxClient.getFolderItems(boxContext, boxContext.getCurrentFolder).onSuccess{
              case boxFolderItems =>
                sendSuggestion(boxFolderItems, params)
                context become(getBehavior(Some(boxFolderItems)), true)
            }
        }
      }
  }

  def sendSuggestion(boxFolderItems:BoxFolderItems, input:String) = {
    val candidates = boxFolderItems.items.filter(_.itemType == BoxItemType.Folder).filter(_.name.startsWith(input))
    if (candidates.size > 0){

      Future{
        val msg = JsObject(
          Seq(
            "text" -> JsString("cd " + StringUtils.diff(candidates.map(_.name)))
          )
        )

        myChannel.push(msg)
      }
    }
  }

//  def receive = {
//    case Connect() => sender ! myEnumerator
//    case Command(input) =>
//      //todo svl 04 Nov 13 (8:15 PM) vspivak: Fix the following hacky code
//      if (input.startsWith("cd")) {
//        val params = input.substring(2).trim
//
//        boxContext.boxClient.getFolderItems(boxContext, boxContext.getCurrentFolder).onSuccess{
//          case items => val candidates = items.items.filter(_.itemType == BoxItemType.Folder).filter(_.name.startsWith(params))
//            if (candidates.size > 0){
//
//              Future{
//                val msg = JsObject(
//                  Seq(
//                    "text" -> JsString("cd " + StringUtils.diff(candidates.map(_.name)))
//                  )
//                )
//
//                myChannel.push(msg)
//            }
//          }
//        }
//      }
//  }
}

case class Connect()
case class Command(command:String)

