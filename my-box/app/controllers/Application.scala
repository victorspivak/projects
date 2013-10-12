package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Await
import lib._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.commands.{BoxContext}
import play.api.data.Forms._
import play.api.data.Form

object Application extends Controller {
  val inputCommandForm = Form[String]("command" -> text)

  def start = Action {request =>
    try {
      val bc = BoxContext(request.session)

      val folderId = "0"
      val rootItemsFuture = bc.getFolderItems(folderId)
      val rootFuture = bc.getFolder(folderId)
      val userFuture = bc.getUser("me")

      val session = request.session

      AsyncResult{
        for {
          root <- rootFuture
          rootItems <- rootItemsFuture
          user <- userFuture
          tokens <- bc.token
        } yield  Ok(views.html.folder(inputCommandForm)(user, root, rootItems)).withSession(
          "accessToken" -> tokens.accessToken, "refreshToken" -> tokens.refreshToken)
      }
    } catch {
      case e:Exception => println(e)
        Ok(views.html.box(inputCommandForm)(e.getMessage))
    }
  }

  def processCommand() = Action {implicit request =>
    val command = inputCommandForm.bindFromRequest().data("command")

    val accessToken = session.get("accessToken")
    val refreshToken = session.get("refreshToken")

    val bc = BoxContext(request.session)

    val folderId = command
    val rootItemsFuture = bc.getFolderItems(folderId)
    val rootFuture = bc.getFolder(folderId)
    val userFuture = bc.getUser("me")

   AsyncResult{
     for {
       root <- rootFuture
       rootItems <- rootItemsFuture
       user <- userFuture
     } yield  Ok(views.html.folder(inputCommandForm)(user, root, rootItems))
   }
  }
}
