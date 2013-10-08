package controllers

import play.api._
import play.api.mvc._
import scala.concurrent.Await
import lib._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  def index = Action.async {
//    val boxConfig = new BoxAppConfig("https://api.feature01.inside-box.net/api", "https://api.feature01.inside-box.net/api/2.0",
//      "a3nhdenqgvp4q7b2ujj12nu8te0sbsma", "4tgqXEPrtohvakK3jv2LD2rJWtFaiGZx")
//    val resF = bc.authenticate("mwiller+dev2@box.com", "test1234")

    val boxConfig = new BoxAppConfig("https://vspivak.inside-box.net/api",
      "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU")
    val bc = BoxClient(boxConfig)
    implicit val tokenFuture = bc.authenticate("vic+admin@box.com", "test1234")

//    val rootFuture = bc.getFolder("0")
//    rootFuture.map{root =>
//      Ok(views.html.box(s"Root folder: $root"))
//    }

//    val rootFuture = bc.getFolderItems("0")
//    rootFuture.map{root =>
//      Ok(views.html.box(s"Root folder items: $root"))
//    }

    val rootItemsFuture = bc.getFolderItems("0")
    val rootFuture = bc.getFolder("0")
    val userFuture = bc.getUser("me")

    for {
      root <- rootFuture
      rootItems <- rootItemsFuture
      user <- userFuture
    } yield  Ok(views.html.rootFolder(user, root, rootItems))
  }
}
