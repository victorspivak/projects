package controllers

import play.api.mvc._
import play.api.data.Forms._
import play.api.data.Form
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.ShowFolder
import lib.{CommandParser, BoxAppConfig, BoxAuthenticator, BoxClient}

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)

  def start = Action {implicit request =>
    AsyncResult{
      BoxContext.fromRequest(request) match {
        case Some(context) => ShowFolder("0").execute(context)
        case None => Future(BoxAuthenticator(BoxClient.config).getOauth2Code)
      }
    }
  }

  def test = Action {
    BoxAuthenticator(BoxClient.config).getOauth2Code
  }

  //svl add error handling - when it did not get token
  def authtoken = Action {implicit request =>
    AsyncResult{
      BoxContext.fromRequest(request) match {
        case Some(context) => ShowFolder("0").execute(context)
        case None => Future(BoxAuthenticator(BoxClient.config).getOauth2Code)
      }
    }
  }

  def processCommand() = Action {implicit request =>
    val command = inputCommandForm.bindFromRequest().data("command")
    AsyncResult{
      BoxContext.fromRequest(request) match {
        case Some(context) => CommandParser.parse(command).execute(context)
        case None => Future(BoxAuthenticator(BoxClient.config).getOauth2Code)
      }
    }
  }
}
