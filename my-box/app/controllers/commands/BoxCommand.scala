package controllers.commands

import play.api.mvc.SimpleResult
import scala.concurrent.Future


trait BoxCommand extends play.api.mvc.Results{
  def execute: Future[SimpleResult]
}
