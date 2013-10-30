package controllers.commands

import lib.{FolderService, BoxClient}
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BoxContext

class ShowCurrentFolder extends BoxCommand {
  def execute(context:BoxContext) = {
    val folderId = context.getCurrentFolder
    context.toSessionData.flatMap{sessionData =>
      FolderService().fetchFolderData(context, folderId) map {folderData =>
        Ok(views.html.folder(folderData)).withSession(sessionData: _*)
      }
    }
  }
}

object ShowCurrentFolder {
  def apply() = new ShowCurrentFolder
}

class CdCommand(val path:String) extends BoxCommand {
  def execute(context:BoxContext) = {
    FolderService().cd(context, path).flatMap{newContext=>
      ShowCurrentFolder().execute(newContext)
    }
  }
}
