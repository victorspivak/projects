package controllers

import play.api.mvc._
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

  def fromRequest(request:Request[AnyContent]) = {
    val session = request.session

    val tokenOpt = for {
      accessToken <- session.get(ACCESS_TOKEN)
      refreshToken <- session.get(REFRESH_TOKEN)
    } yield BoxToken(accessToken, refreshToken)

    //svl find a better home for BoxConfig
    val tokenAsFuture = tokenOpt match {
      case Some(t) => Option(Future(t))
      case None =>  retrieveTokenUsingCode(request)
    }

    tokenAsFuture.map{token =>
      BoxContext(token, session.get(CURRENT_FOLDER).getOrElse("0"))
    }
  }

  private def retrieveTokenUsingCode(request:Request[AnyContent]) = request.queryString.get("code").map {code =>
    BoxAuthenticator(BoxClient.config).authenticate(code.toList.head)
  }
}
