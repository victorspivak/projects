package lib

import model.FolderData
import scala.concurrent.ExecutionContext.Implicits.global

object FolderService {
  def fetchFolderData(folderId:String, boxClient:BoxClient) = {
    val rootItemsFuture = boxClient.getFolderItems(folderId)
    val rootFuture = boxClient.getFolder(folderId)
    val userFuture = boxClient.getUser("me")

    for {
      root_ <- rootFuture
      rootItems_ <- rootItemsFuture
      user_ <- userFuture
    } yield for {
        root <- root_
        rootItems <- rootItems_
        user <- user_
      } yield FolderData(user, root, rootItems)
  }
}
