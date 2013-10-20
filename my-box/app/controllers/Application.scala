package controllers

import play.api.mvc._
import play.api.data.Forms._
import play.api.data.Form
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.ShowFolder
import lib._
import play.api.mvc.AsyncResult
import scala.Some
import java.net.ConnectException

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)
  val boxClient = BoxClient(BoxAppConfig("https://vspivak.inside-box.net/api", "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU"))

  def test = Action {implicit request =>
    Ok(views.html.message("Message:", "" + routes.Application.authtoken.absoluteURL(true)))
  }

  def authtoken = Action {implicit request =>
    AsyncResult{
      BoxContext.fromRequest(boxClient, request) match {
        case Some(context) => ShowFolder("0").execute(context)
        case None => Future(boxClient.boxAuthenticator.getOauth2Code)
      }
    }
  }

  def start = Action {implicit request =>
    lazy val command = ShowFolder("0")

    execAsync(request, command.execute)
  }

  def processCommand() = Action {implicit request =>
    lazy val commandString = inputCommandForm.bindFromRequest().data("command")
    lazy val command = CommandParser.parse(commandString)

    execAsync(request, command.execute)
  }

  private def execAsync(request:Request[AnyContent], func:BoxContext=>Future[Result]) = {
    AsyncResult{
      BoxContext.fromRequest(boxClient, request) match {
        case Some(context) => exceptionHandler(context, func)
        case None => Future(boxClient.boxAuthenticator.getOauth2Code(request))
      }
    }
  }

  private def exceptionHandler(context:BoxContext, func:BoxContext=>Future[Result]) = {
    func(context).recoverWith{
      case e:ConnectException =>
        Future(Ok(views.html.message("Could not connect to the server", e.getMessage)))
      case BoxHttpErrorException(401, _, _) =>func(BoxContext.refreshToken(context)).recover{
          case e => Ok(views.html.message(e.getMessage)).withNewSession
        }
      case e:Exception =>
        Future(Ok(views.html.message("There is an error", e.getMessage)))
    }
  }
}
