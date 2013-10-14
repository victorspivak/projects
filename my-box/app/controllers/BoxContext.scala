package controllers

import lib.{BoxAuthenticator, BoxToken, BoxClient, BoxAppConfig}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc.Session

case class BoxContext(tokenFuture:Future[BoxToken], currentFolderId:String) {
  def setCurrentFolder(currentFolderId:String) = BoxContext(tokenFuture, currentFolderId)

  def toSessionData = {
    tokenFuture.map {token =>
      List( BoxContext.ACCESS_TOKEN -> token.accessToken,
            BoxContext.REFRESH_TOKEN -> token.refreshToken,
            BoxContext.CURRENT_FOLDER -> currentFolderId
    )}
  }
}

object BoxContext {
  val ACCESS_TOKEN:   String = "accessToken"
  val REFRESH_TOKEN:  String = "refreshToken"
  val CURRENT_FOLDER: String = "currentFolder"

  def fromSession(session:Session, config:BoxAppConfig) = {
    val tokenOpt = for {
      accessToken <- session.get(ACCESS_TOKEN)
      refreshToken <- session.get(REFRESH_TOKEN)
    } yield BoxToken(accessToken, refreshToken)

    val token = tokenOpt match {
      case Some(t) => Future(t)
      case None =>   BoxAuthenticator(config).authenticate("vic+admin@box.com", "test1234")
    }

    BoxContext(token, session.get(CURRENT_FOLDER).getOrElse("0"))
  }
}
