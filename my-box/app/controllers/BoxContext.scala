package controllers

import play.api.mvc._
import scala.util.{Success, Failure}
import lib.{BoxAuthenticator, BoxToken, BoxClient, BoxAppConfig}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.mvc.Session

case class BoxContext(boxClient:BoxClient, tokenFuture:Future[BoxToken], currentFolderId:String) {
  def setCurrentFolder(currentFolderId:String) = BoxContext(boxClient, tokenFuture, currentFolderId)

  def toSessionData = tokenFuture.map (buildSessionData)
  def toSessionDataOpt = tokenFuture.value.flatMap {
    case Success(token) => Some(buildSessionData(token))
    case Failure(e) => None
  }

  private def buildSessionData(token: BoxToken): List[(String, String)] = {
    List(BoxContext.ACCESS_TOKEN -> token.accessToken,
      BoxContext.REFRESH_TOKEN -> token.refreshToken,
      BoxContext.CURRENT_FOLDER -> currentFolderId
    )
  }
}

object BoxContext {
  val ACCESS_TOKEN:   String = "accessToken"
  val REFRESH_TOKEN:  String = "refreshToken"
  val CURRENT_FOLDER: String = "currentFolder"

  def fromRequest(boxClient:BoxClient, request:Request[AnyContent]) = {
    val session = request.session

    val tokenOpt = for {
      accessToken <- session.get(ACCESS_TOKEN)
      refreshToken <- session.get(REFRESH_TOKEN)
    } yield BoxToken(accessToken, refreshToken)

    val tokenAsFuture = tokenOpt match {
      case Some(t) => Option(Future(t))
      case None =>  retrieveTokenUsingCode(boxClient, request)
    }

    tokenAsFuture.map{token =>
      BoxContext(boxClient, token, session.get(CURRENT_FOLDER).getOrElse("0"))
    }
  }

  def refreshToken(context:BoxContext) = {
    val newToken = context.boxClient.boxAuthenticator.refresh(context.tokenFuture.value.get.get)
    BoxContext(context.boxClient, newToken, context.currentFolderId)
  }

  private def retrieveTokenUsingCode(boxClient:BoxClient, request:Request[AnyContent]) =
    request.queryString.get("code").map {code => boxClient.boxAuthenticator.authenticate(code.toList.head)
  }
}
