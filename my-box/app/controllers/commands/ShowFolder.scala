package controllers.commands

import lib.{FolderService, BoxClient}
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BoxContext

class ShowFolder(val folderId:String) extends BoxCommand {
  def execute(context:BoxContext) = {
    val boxClient = BoxClient(context)
    context.setCurrentFolder(folderId).toSessionData.flatMap{sessionData =>
      FolderService.fetchFolderData(folderId, boxClient) map {folderData =>
        Ok(views.html.folder(folderData)).withSession(sessionData: _*)
      }
    }
  }
}

object ShowFolder {
  def apply(folderId:String) = new ShowFolder(folderId)
}


