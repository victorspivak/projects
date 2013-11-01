package controllers

import play.api.mvc._
import play.api.data.Forms._
import play.api.data.Form
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.ShowCurrentFolder
import lib._
import java.net.ConnectException
import play.api.mvc.AsyncResult
import lib.BoxAppConfig
import scala.Some

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)
//  val boxClient = BoxClient(BoxAppConfig("https://vspivak.inside-box.net/api", "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU"))
  val boxClient = BoxClient(BoxAppConfig("https://www.box.com/api", "7j2k31matzekl35p5mit1x6mz8si9wbp", "6qkDs9bpt2EXJlMYHa2FxgSisOFgJxhW", "https://victorspivak.homeip.net:9091/authtoken"))

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
        //Future(Ok(views.html.message(e.getMessage)))
      case BoxHttpErrorException(401, _, _) =>func(BoxContext.refreshToken(context)).recover{
          case e => Ok(views.html.message(e.getMessage)).withNewSession
        }
      case e:Exception =>
        Future(Ok(views.html.message("There is an error", e.getMessage)))
    }
  }
}
