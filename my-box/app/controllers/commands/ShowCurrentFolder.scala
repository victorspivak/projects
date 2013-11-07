package controllers.commands

import lib.{FolderService, BoxClient}
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BoxContext
import play.api.mvc.RequestHeader
import scala.concurrent.Future
import play.api.mvc.Results._

class ShowCurrentFolder extends BoxCommand {
  def execute(context:BoxContext) = {
    val folderId = context.getCurrentFolder
    context.toSessionData.flatMap{sessionData =>
      FolderService().fetchFolderData(context, folderId) map {folderData =>
        Ok(views.html.folder(context, folderData)(context.request)).withSession(sessionData: _*)
      }
    }
  }

  def autoComplete(autoCompleter:AutoCompleter): Future[Option[String]] = Future.successful(None)
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

  def autoComplete(autoCompleter:AutoCompleter): Future[Option[String]] = {
    autoCompleter.completePath(path).map(_.map("cd " + _))
  }
}
