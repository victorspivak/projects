package controllers

import play.api.mvc._
import play.api.data.Forms._
import play.api.data.Form
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.ShowCurrentFolder
import lib._
import play.api.mvc.AsyncResult
import scala.Some
import java.net.ConnectException
import play.api.mvc.AsyncResult
import lib.BoxAppConfig
import scala.Some

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)
  val boxClient = BoxClient(BoxAppConfig("https://vspivak.inside-box.net/api", "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU"))

  def test = Action {implicit request =>
    Ok(views.html.message("Message:", "" + routes.Application.authtoken.absoluteURL(true)))
//    execAsync(request, {context:BoxContext =>
//      FolderService().cd(context, "BOX-73892").flatMap{newContext=>
//        ShowCurrentFolder().execute(newContext)
//      }
//    })
  }

  def authtoken = Action {implicit request =>
    AsyncResult{
      BoxContext.fromRequest(boxClient, request) match {
        case Some(context) => ShowCurrentFolder().execute(context)
        case None => Future(boxClient.boxAuthenticator.getOauth2Code)
      }
    }
  }

  def start = Action {implicit request =>
    execAsync(request, {context=>
          ShowCurrentFolder().execute(context)
    })
  }

  def processCommand() = Action {implicit request =>
    execAsync(request, {context=>
      val commandString = inputCommandForm.bindFromRequest().data("command")
      val command = CommandParser.parse(commandString)
      command.execute(context)
    })
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
      case e:BoxFolderNotFoundException =>
         ShowCurrentFolder().execute(context.setStatus(e.getMessage))
        //Future(Ok(views.html.message(e.getMessage)))
      case BoxHttpErrorException(401, _, _) =>func(BoxContext.refreshToken(context)).recover{
          case e => Ok(views.html.message(e.getMessage)).withNewSession
        }
      case e:Exception =>
        Future(Ok(views.html.message("There is an error", e.getMessage)))
    }
  }
}
