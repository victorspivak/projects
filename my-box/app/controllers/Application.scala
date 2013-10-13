package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Await
import lib._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.{BoxContext}
import play.api.data.Forms._
import play.api.data.Form
import scala.util.{Try, Success, Failure}
import scala.concurrent.Future
import model.FolderData

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)

  def start = Action {implicit request =>
    showFolder("0")
  }

  def processCommand() = Action {implicit request =>
    val command = inputCommandForm.bindFromRequest().data("command")
    showFolder(command)
  }

  private def showFolder(folderId:String)(implicit request:Request[AnyContent]) = {
    AsyncResult{
      val boxClient = BoxClient(request.session)

      boxClient.boxContext.setCurrentFolder(folderId).toSessionData.flatMap{sessionData =>
        FolderService.fetchFolderData(folderId, boxClient) map{folderDataTry =>
          folderDataTry match {
            case Success(folderData) => Ok(views.html.folder(inputCommandForm)(folderData)).withSession(sessionData: _*)
            case Failure(e) => Ok(views.html.box(inputCommandForm)(e.getMessage)).withNewSession
          }
        }
      }
    }
  }
}
