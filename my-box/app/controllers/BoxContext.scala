package controllers

import play.api.mvc._
import scala.util.{Success, Failure}
import lib.{BoxToken, BoxClient,BoxFolderNotFoundException}
import scala.concurrent.{Await, Future}
import scala.collection.immutable.Stack
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

case class BoxContext(request:RequestHeader, boxClient:BoxClient, tokenFuture:Future[BoxToken], userName:Option[String], currentPath:Stack[String], statusMessage:String = "") {
  def pushFolder(folderId:String) = BoxContext(request, boxClient, tokenFuture, userName, currentPath.push(folderId))
  def popFolder = {
    if (currentPath.size < 2)
      throw new BoxFolderNotFoundException("..", "The current folder is root")
    BoxContext(request, boxClient, tokenFuture, userName, currentPath.pop)
  }

  def setRootFolder() = BoxContext(request, boxClient, tokenFuture, userName, Stack("0"))

  def setStatus(message:String) = BoxContext(request, boxClient, tokenFuture, userName, currentPath, message)
  def getCurrentFolder = currentPath.top

  def toSessionData = tokenFuture.map (buildSessionData)
  def toSessionDataOpt = tokenFuture.value.flatMap {
    case Success(token) => Some(buildSessionData(token))
    case Failure(e) => None
  }

  private def buildSessionData(token: BoxToken): List[(String, String)] = {
    val data = List(BoxContext.ACCESS_TOKEN -> token.accessToken,
      BoxContext.REFRESH_TOKEN -> token.refreshToken,
      BoxContext.CURRENT_PATH -> currentPath.mkString(BoxContext.PATH_DELIMITER)
    )

    userName.map(name => BoxContext.USER_NAME ->name :: data).getOrElse(data)
  }
}

object BoxContext {
  val ACCESS_TOKEN:   String = "accessToken"
  val REFRESH_TOKEN:  String = "refreshToken"
  val CURRENT_PATH:   String = "currentPath"
  val USER_NAME:      String = "userName"
  val PATH_DELIMITER: String = ","

  def fromRequest(boxClient:BoxClient, request:RequestHeader) = {
    val session = request.session

    val tokenOpt = for {
      accessToken <- session.get(ACCESS_TOKEN)
      refreshToken <- session.get(REFRESH_TOKEN)
    } yield BoxToken(accessToken, refreshToken)

    val tokenAsFuture = tokenOpt match {
      case Some(t) => Option(Future(t))
      case None =>  retrieveTokenUsingCode(boxClient, request)
    }

    val pathStack = Stack(session.get(CURRENT_PATH).getOrElse("0").split(PATH_DELIMITER): _*)

    tokenAsFuture.map{token =>
      val userName = session.get(USER_NAME) match {
        case Some(name) => Some(name)
        case None =>
          val tempContext = BoxContext(request, boxClient, token, None, pathStack)
          val user = boxClient.getUser(tempContext, "me")
          Await.ready(user, 5 seconds)
          user.value.flatMap {
            case Success(user_) => Option(user_.name)
            case Failure(_) => None
          }
      }

      BoxContext(request, boxClient, token, userName, pathStack)
    }
  }

  def refreshToken(context:BoxContext) = {
    val newToken = context.boxClient.boxAuthenticator.refresh(context.tokenFuture.value.get.get)
    BoxContext(context.request, context.boxClient, newToken, context.userName, context.currentPath)
  }

  private def retrieveTokenUsingCode(boxClient:BoxClient, request:RequestHeader) =
    request.queryString.get("code").map {code => boxClient.boxAuthenticator.authenticate(code.toList.head)
  }
}
