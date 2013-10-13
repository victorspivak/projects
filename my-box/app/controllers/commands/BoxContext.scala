package controllers.commands

import lib.{BoxToken, BoxClient, BoxAppConfig}
import scala.concurrent.ExecutionContext.Implicits.global

case class BoxContext(boxClient:BoxClient, currentFolderId:String) {
  def setCurrentFolder(currentFolderId:String) = BoxContext(boxClient, currentFolderId)

  def toSessionData = {
    boxClient.token.map {tokens =>
      List( "accessToken" -> tokens.accessToken,
            "refreshToken" -> tokens.refreshToken,
            "currentFolder" -> currentFolderId
    )}
  }
}
