package lib

import scala.{Some, None}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import model.FolderData
import controllers.BoxContext
import model.BoxItem
import model.BoxItem.BoxItemType

class FolderService {
  def fetchFolderData(context:BoxContext, folderId:String) = {
    val rootItemsFuture = context.boxClient.getFolderItems(context, folderId)
    val rootFuture = context.boxClient.getFolder(context, folderId)
    val userFuture = context.boxClient.getUser(context, "me")

    for {
      root <- rootFuture
      rootItems <- rootItemsFuture
      user <- userFuture
    } yield FolderData(user, root, rootItems)
  }

  def cd(context:BoxContext, path:String) = makeContextForPath(Future(context), path)

  private def makeContextForPath(contextFuture:Future[BoxContext], path:String):Future[BoxContext] = {
		path.trim match {
			case "" => contextFuture
			case "/" => contextFuture.map(_.setRootFolder())
			case p if p.startsWith("/") =>  makeContextForPath(contextFuture.map(_.setRootFolder()), p.substring(1))
			case relativePath => 
        val PathTemplate = """([^/]+)/(.*)""".r
        relativePath match {
      			case PathTemplate("..", restPath) => makeContextForPath(contextFuture.map(_.popFolder), restPath) 
      			case PathTemplate(folder, restPath) =>
              makeContextForPath(makeContextForFolder(contextFuture, folder), restPath)
            case ".." => contextFuture.map(_.popFolder)
            case folder => makeContextForFolder(contextFuture, folder)
		    }
    }
  }

  def makeContextForFolder(contextFuture: Future[BoxContext], folder: String): Future[BoxContext] = {
      contextFuture.flatMap {context =>
      folderIdByName(context, folder).map {item =>
        context.pushFolder(item.id)
      }
    }
  }

  private def folderIdByName(context:BoxContext, name:String) = {
    val boxClient = context.boxClient
    boxClient.getFolderItems(context, context.getCurrentFolder).flatMap{folderItems=>
      val foundFolder = folderItems.items.find{item=>
        item.name == name && item.itemType == BoxItemType.Folder
      }

      foundFolder match {
        case Some(item) => Future.successful(item)
        case None => Future.failed(new BoxFolderNotFoundException(name))
      }
    }
  }
}

object FolderService {
  def apply() = new FolderService
}
