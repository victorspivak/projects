package controllers

import play.api.mvc._
import play.api.data.Forms._
import play.api.data.Form
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.ShowCurrentFolder
import lib._
import java.net.ConnectException
import lib.BoxAppConfig
import scala.Some
import play.api.{Logger, Play}
import play.api.libs.json.JsValue

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)

  val boxClient = BoxClient(buildAppConfig)

  def test = Action {implicit request =>
    Ok(views.html.message("Message:", "" + routes.Application.authtoken().absoluteURL(secure = true)))
  }

  def authtoken = Action.async{implicit request =>
    BoxContext.fromRequest(boxClient, request) match {
      case Some(context) => ShowCurrentFolder().execute(context)
      case None => Future(boxClient.boxAuthenticator.getOauth2Code)
    }
  }

  def start = Action.async{implicit request =>
    execute(request, {context=>ShowCurrentFolder().execute(context)
    })
  }

  def processCommand() = Action.async {implicit request =>
    execute(request, {context=>
      val commandString = inputCommandForm.bindFromRequest().data("command")
      val command = CommandParser.parse(commandString)
      command.execute(context)
    })
  }

  def startAutoCompleter = WebSocket.async[JsValue] { request  =>
    BoxContext.fromRequest(boxClient, request) match {
      case Some(context) =>     AutoCompleteService.start(context)
      case None => throw new Exception("Could not get context")
    }
  }

  private def execute(request:Request[AnyContent], func:BoxContext=>Future[SimpleResult]) = {
    BoxContext.fromRequest(boxClient, request) match {
      case Some(context) => exceptionHandler(context, func)
      case None => Future(boxClient.boxAuthenticator.getOauth2Code(request))
    }
  }

  private def exceptionHandler(context:BoxContext, func:BoxContext=>Future[SimpleResult]) = {
    func(context).recoverWith{
      case e:ConnectException =>
        Future(Ok(views.html.message("Could not connect to the server", e.getMessage)))
      case e:BoxFolderNotFoundException =>
         ShowCurrentFolder().execute(context.setStatus(e.getMessage))
      case BoxHttpErrorException(401, _, _) =>func(BoxContext.refreshToken(context)).recover{
          case e => Ok(views.html.message(e.getMessage)).withNewSession
        }
      case e:Exception =>
        BoxClient.logger.error("Error", e)
        Future(Ok(views.html.message("There is an error", e.getMessage)))
    }
  }

  private def buildAppConfig = {
    val appConfig = for {
      apiUrl <- Play.current.configuration.getString("box.api.url")
      clientId <- Play.current.configuration.getString("box.client.id")
      clientSecret <- Play.current.configuration.getString("box.client.secret")
    } yield BoxAppConfig(apiUrl, clientId, clientSecret)

    if (appConfig.isEmpty){
      Logger.error("Box Application is not configured")
      throw new Exception("Box Application is not configured")
    }

    appConfig.get
  }
}
