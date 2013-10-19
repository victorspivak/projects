package lib

import play.api.libs.ws._
import scala.concurrent.Future
import model._
import model.BoxFolderResource
import play.api.libs.json.JsValue
import controllers.BoxContext
import model.BoxItem.BoxItemType
import scala.concurrent.ExecutionContext.Implicits.global

case class BoxAppConfig(url:String, clientId:String, clientSecret:String)

case class BoxToken(accessToken:String, refreshToken:String)

class BoxClient(val boxContext:BoxContext) {
  def getUser(id:String) = {
    get(BoxUserResource(id)).map((new Json2BoxUser).toEntity)
  }

  def getFolder(id:String)= {
    get(BoxFolderResource(id)).map((new Json2BoxFolder).toEntity)
  }

  def getFolderItems(id:String) = {
    get(BoxFolderItemsResource(id)).map(new Json2BoxFolderItems(id).toEntity)
  }

  def folderIdByName(name:String) = {
    getFolderItems(boxContext.currentFolderId).map{folderItems=>
      folderItems.items.find{item=>
        item.name == name && item.itemType == BoxItemType.Folder
      }.get
    }
  }

  private def get[T <: BoxEntity](resource: BoxResource[T]): Future[JsValue] = {
    val fullUrl = apiUrl + resource.path
    boxContext.tokenFuture flatMap {token =>
      System.out.println(s"Sending request to $fullUrl")
      val getFuture = WS.url(fullUrl).withHeaders(getAuthHeader(token)).withQueryString(resource.getParams: _*).get()
      getFuture flatMap {
        response => {
          response.status match {
            case 200 =>
              System.out.println(s"Request $fullUrl is completed")
              Future.successful(response.json)
            case _ => Future.failed(BoxHttpErrorException(response))
          }
        }
      }
    }
  }

  private def apiUrl:   String = BoxClient.config.url + "/2.0"
  private def getAuthHeader(token:BoxToken) = "Authorization" -> s"Bearer ${token.accessToken}"
}

object BoxClient {
  val config = new BoxAppConfig("https://vspivak.inside-box.net/api",
        "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU")

  def apply(boxContext:BoxContext) = new BoxClient(boxContext) : BoxClient
}
