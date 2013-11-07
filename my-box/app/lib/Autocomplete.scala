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
import controllers.commands.{BoxCommand, AutoCompleter}
import scala.concurrent.Future
import model.BoxFolderItems
import scala.Some
import play.api.libs.json.JsString
import play.api.libs.json.JsObject
import model.BoxItem.BoxItemType.BoxItemType
import model.BoxItem.BoxItemType

object AutoCompleteAgent {
  implicit val timeout = Timeout(1 second)
  val logger = LoggerFactory.getLogger("AutoCompleteService")

  val loggerIteratee = Iteratee.foreach[JsValue](event => logger.debug(s"${event.toString()}"))

  def start(boxContext:BoxContext) = {
    val props = Props(classOf[AutoCompleteAgent], boxContext)
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

class AutoCompleteAgent(boxContext:BoxContext) extends Actor {
  val (myEnumerator, myChannel) = Concurrent.broadcast[JsValue]

  def receive = getBehavior(None)
  def getBehavior(autoCompleter:Option[AutoCompleter]): Receive = {
    case Connect() => sender ! myEnumerator
    case Command(input) =>
      CommandParser.parseAsFuture(input)(boxContext.request).map{command=>
        autoCompleter match {
          case Some(completer) => sendSuggestion(command, completer)
          case None =>
            boxContext.boxClient.getFolderItems(boxContext, boxContext.getCurrentFolder).onSuccess{
              case boxFolderItems =>
                val completer = new AutoCompleterImp(boxFolderItems)
                sendSuggestion(command, completer)
                context become(getBehavior(Some(completer)), true)
            }
        }
      }
  }

  private def sendSuggestion(command:BoxCommand, completer:AutoCompleter) {
    command.autoComplete(completer).map(_.map(suggestion=>sendSuggestion(suggestion)))
  }

  def sendSuggestion(suggestion:String) {
    val msg = JsObject(
      Seq("text" -> JsString(suggestion))
    )

    myChannel.push(msg)
  }
}

case class Connect()
case class Command(command:String)

class AutoCompleterImp(folderItems:BoxFolderItems) extends AutoCompleter{
  def completePath(path: String) = filter(path, BoxItemType.Folder)
  def completeFilename(filename: String) = filter(filename, BoxItemType.Folder)

  def filter(name: String, itemType:BoxItemType) = {
    val candidates = folderItems.items.filter(_.itemType == itemType).filter(_.name.startsWith(name))
    if (candidates.size > 0)
      Future.successful(Option(StringUtils.diff(candidates.map(_.name))))
    else
      Future.successful(None)
  }
}
