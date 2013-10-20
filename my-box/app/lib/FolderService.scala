package lib

import model.FolderData
import scala.concurrent.ExecutionContext.Implicits.global
import controllers.BoxContext

object FolderService {
  def fetchFolderData(boxContext:BoxContext, folderId:String) = {
    val rootItemsFuture = boxContext.boxClient.getFolderItems(boxContext, folderId)
    val rootFuture = boxContext.boxClient.getFolder(boxContext, folderId)
    val userFuture = boxContext.boxClient.getUser(boxContext, "me")

    for {
      root <- rootFuture
      rootItems <- rootItemsFuture
      user <- userFuture
    } yield FolderData(user, root, rootItems)
  }
}
