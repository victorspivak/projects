package lib

import model.FolderData
import scala.concurrent.ExecutionContext.Implicits.global

object FolderService {
  def fetchFolderData(folderId:String, boxClient:BoxClient) = {
    val rootItemsFuture = boxClient.getFolderItems(folderId)
    val rootFuture = boxClient.getFolder(folderId)
    val userFuture = boxClient.getUser("me")

    for {
      root <- rootFuture
      rootItems <- rootItemsFuture
      user <- userFuture
    } yield FolderData(user, root, rootItems)
  }
}
