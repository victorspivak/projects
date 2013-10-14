package controllers

import play.api.mvc._
import play.api.data.Forms._
import play.api.data.Form
import controllers.commands.ShowFolder
import lib.CommandParser

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)

  def start = Action {implicit request =>
    AsyncResult{
      ShowFolder("0").execute
    }
  }

  def processCommand() = Action {implicit request =>
    val command = inputCommandForm.bindFromRequest().data("command")
    AsyncResult{
      CommandParser.parse(command).execute
    }
  }
}
