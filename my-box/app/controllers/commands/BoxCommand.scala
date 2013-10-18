package controllers.commands

import play.api.mvc.SimpleResult
import scala.concurrent.Future
import controllers.BoxContext


trait BoxCommand extends play.api.mvc.Results{
  def execute(context:BoxContext): Future[SimpleResult]
}
