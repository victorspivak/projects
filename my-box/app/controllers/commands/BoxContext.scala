package controllers.commands

import lib.{BoxToken, BoxClient, BoxAppConfig}
import play.api.mvc.Session

object BoxContext {
  def apply(implicit session:Session) = {
//    val boxConfig = new BoxAppConfig("https://api.feature01.inside-box.net/api", "https://api.feature01.inside-box.net/api/2.0",
//      "a3nhdenqgvp4q7b2ujj12nu8te0sbsma", "4tgqXEPrtohvakK3jv2LD2rJWtFaiGZx")
//    val resF = bc.authenticate("mwiller+dev2@box.com", "test1234")

    val boxConfig = new BoxAppConfig("https://vspivak.inside-box.net/api",
      "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU")

    val token = for {
      accessToken <- session.get("accessToken")
      refreshToken <- session.get("refreshToken")
    } yield BoxToken(accessToken, refreshToken)

    BoxClient(boxConfig, token)
  }
}
