package controllers

import play.api.mvc._
import scala.util.{Success, Failure}
import lib.{BoxToken, BoxClient,BoxFolderNotFoundException}
import scala.concurrent.Future
import scala.collection.immutable.Stack
import scala.concurrent.ExecutionContext.Implicits.global

case class BoxContext(boxClient:BoxClient, tokenFuture:Future[BoxToken], currentPath:Stack[String], statusMessage:String = "") {
  def pushFolder(folderId:String) = BoxContext(boxClient, tokenFuture, currentPath.push(folderId))
  def popFolder = {
    if (currentPath.size < 2)
      throw new BoxFolderNotFoundException("..", "The current folder is root")
    BoxContext(boxClient, tokenFuture, currentPath.pop)
  }

  def setRootFolder() = BoxContext(boxClient, tokenFuture, Stack("0"))

  def setStatus(message:String) = BoxContext(boxClient, tokenFuture, currentPath, message)
  def getCurrentFolder = currentPath.top

  def toSessionData = tokenFuture.map (buildSessionData)
  def toSessionDataOpt = tokenFuture.value.flatMap {
    case Success(token) => Some(buildSessionData(token))
    case Failure(e) => None
  }

  private def buildSessionData(token: BoxToken): List[(String, String)] = {
    List(BoxContext.ACCESS_TOKEN -> token.accessToken,
      BoxContext.REFRESH_TOKEN -> token.refreshToken,
      BoxContext.CURRENT_PATH -> currentPath.mkString(BoxContext.PATH_DELIMITER)
    )
  }
}

object BoxContext {
  val ACCESS_TOKEN:   String = "accessToken"
  val REFRESH_TOKEN:  String = "refreshToken"
  val CURRENT_PATH:   String = "currentPath"
  val PATH_DELIMITER: String = ","

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
      BoxContext(boxClient, token, Stack(((session.get(CURRENT_PATH).getOrElse("0")).split(PATH_DELIMITER)): _*))
    }
  }

  def refreshToken(context:BoxContext) = {
    val newToken = context.boxClient.boxAuthenticator.refresh(context.tokenFuture.value.get.get)
    BoxContext(context.boxClient, newToken, context.currentPath)
  }

  private def retrieveTokenUsingCode(boxClient:BoxClient, request:Request[AnyContent]) =
    request.queryString.get("code").map {code => boxClient.boxAuthenticator.authenticate(code.toList.head)
  }
}
