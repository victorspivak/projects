package lib

import play.api.mvc.AsyncResult

//class FolderService {
//  def getFolder(folderId:String)(implicit boxClient:BoxClient) = {
//
//    val rootItemsFuture = boxClient.getFolderItems(folderId)
//    val rootFuture = boxClient.getFolder(folderId)
//    val userFuture = boxClient.getUser("me")
//
//   AsyncResult{
//     for {
//       root <- rootFuture
//       rootItems <- rootItemsFuture
//       user <- userFuture
//     } yield  Ok(views.html.folder(inputCommandForm)(user, root, rootItems))
//   }
//
//  }
//}
