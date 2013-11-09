package controllers.commands

import lib._
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BoxContext
import play.api.mvc.{SimpleResult, RequestHeader}
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

class MkDirCommand(val name:String) extends BoxCommand {
  def execute(context:BoxContext) = {
    context.boxClient.mkFolder(context, context.getCurrentFolder, name).flatMap{json=>
      ShowCurrentFolder().execute(context)
    }.recoverWith{
      case e:BoxHttpErrorException =>ShowCurrentFolder().execute(context.setStatus(e.statusText))
      case e:Exception =>ShowCurrentFolder().execute(context.setStatus(e.getMessage))
    }
  }

  def autoComplete(autoCompleter:AutoCompleter): Future[Option[String]] = {
    Future.successful(None)
  }
}

class RmDirCommand(val name:String) extends BoxCommand {
  def execute(context:BoxContext) = {
    FolderService().folderIdByName(context, name).flatMap{item=>
      context.boxClient.rmFolder(context, item.id).flatMap{rez=>
        ShowCurrentFolder().execute(context)
      }
    }.recoverWith{
      case e:BoxHttpErrorException =>ShowCurrentFolder().execute(context.setStatus(e.statusText))
      case e:Exception =>ShowCurrentFolder().execute(context.setStatus(e.getMessage))
    }
  }

  def autoComplete(autoCompleter:AutoCompleter): Future[Option[String]] = {
    autoCompleter.completePath(name).map(_.map("rmdir " + _))
  }
}

class UnknownCommand(val text:String) extends BoxCommand {
    def execute(context:BoxContext): Future[SimpleResult] = {
      throw new UnknownCommandException(text)
  }

  def autoComplete(autoCompleter:AutoCompleter): Future[Option[String]] = {
    Future.successful(StringUtils.diff(UnknownCommand.supportedCommands.filter(_.startsWith(text))) match {
      case "" => None
      case suggestion => Some(suggestion)
    })
  }
}

object UnknownCommand{
  val supportedCommands = List("mkdir", "rmdir", "cd", "ll", "ls")
}
