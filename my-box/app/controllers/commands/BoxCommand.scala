package controllers.commands

import play.api.mvc.{RequestHeader, SimpleResult}
import scala.concurrent.Future
import controllers.BoxContext

trait AutoCompleter {
  def completePath(path:String):Future[Option[String]]
  def completeFilename(filename:String):Future[Option[String]]
}

trait BoxCommand {
  def execute(context:BoxContext): Future[SimpleResult]
  def autoComplete(autoCompleter:AutoCompleter): Future[Option[String]]
}
