package controllers.commands

import lib.{FolderService, BoxClient}
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BoxContext

class ShowFolder(val folderId:String) extends BoxCommand {
  def execute(context:BoxContext) = {
    context.setCurrentFolder(folderId).toSessionData.flatMap{sessionData =>
      FolderService.fetchFolderData(context, folderId) map {folderData =>
        Ok(views.html.folder(folderData)).withSession(sessionData: _*)
      }
    }
  }
}

object ShowFolder {
  def apply(folderId:String) = new ShowFolder(folderId)
}

//class CdCommand(val path:String) extends BoxCommand {
//  def execute(context:BoxContext) = cd(path)
//
// 	def cd(context:BoxContext, path:String) = {
// 		path match {
// 			case "" => return
// 			case "/" => context.reset()
// 			case p if p.startsWith("/") => cd ("/"); cd(p.substring(1))
// 			case restPath => val parts = path.split("/")
// 							parts.foreach(_ match {
// 								case ".." => context.pop()
// 								case part => folderIdByName(part) match {
// 									case Some(id) => context.push((part, id, None))
// 									case None => throw new FolderNotFoundException(part)
// 								}
// 							})
// 		}
// 	}
//
//}
