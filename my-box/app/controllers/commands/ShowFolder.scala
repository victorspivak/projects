package controllers.commands

import play.api.mvc.{AsyncResult, AnyContent, Request}
import lib.{FolderService, BoxClient}
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc.Action

class ShowFolder(val folderId:String)(implicit request:Request[AnyContent])  extends BoxCommand {
  def execute = {
    val boxClient = BoxClient(request.session)

    boxClient.boxContext.setCurrentFolder(folderId).toSessionData.flatMap{sessionData =>
      FolderService.fetchFolderData(folderId, boxClient) map {
        case Success(folderData) => Ok(views.html.folder(folderData)).withSession(sessionData: _*)
        case Failure(e) => Ok(views.html.box(e.getMessage)).withNewSession
      }
    }
  }
}

object ShowFolder {
  def apply(folderId:String)(implicit request:Request[AnyContent]) = new ShowFolder(folderId)
}


