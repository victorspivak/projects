package controllers.commands

import play.api.mvc.{AnyContent, Request}
import lib.{FolderService, BoxClient}
import scala.concurrent.ExecutionContext.Implicits.global
import java.net.ConnectException
import controllers.BoxContext

class ShowFolder(val folderId:String) extends BoxCommand {
  def execute(context:BoxContext) = {
    val boxClient = BoxClient(context)
    context.setCurrentFolder(folderId).toSessionData.flatMap{sessionData =>
      FolderService.fetchFolderData(folderId, boxClient) map {folderData =>
        Ok(views.html.folder(folderData)).withSession(sessionData: _*)
      }
    }.recover{
      case e:ConnectException =>
        Ok(views.html.message("Could not connect to the server", e.getMessage)).withNewSession
      case e =>
        println(">>>>>>>>>>>>>>>>>> I got " + e)
        e.printStackTrace()
        Ok(views.html.message(e.getMessage)).withNewSession
    }
  }
}

object ShowFolder {
  def apply(folderId:String) = new ShowFolder(folderId)
}


