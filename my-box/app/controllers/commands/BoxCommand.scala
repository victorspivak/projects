package controllers.commands

import play.api.mvc.{RequestHeader, SimpleResult}
import scala.concurrent.Future
import controllers.BoxContext

trait BoxCommand {
  def execute(context:BoxContext): Future[SimpleResult]
  def autoComplete(context:BoxContext): Future[Option[String]]
}
