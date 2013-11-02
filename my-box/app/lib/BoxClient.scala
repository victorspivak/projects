package lib

import play.api.libs.ws._
import scala.concurrent.Future
import model._
import model.BoxFolderResource
import play.api.libs.json.JsValue
import controllers.BoxContext
import model.BoxItem.BoxItemType
import scala.concurrent.ExecutionContext.Implicits.global
import play.Logger
import org.slf4j.LoggerFactory

case class BoxAppConfig(url:String, clientId:String, clientSecret:String, redirectUrl:Option[String] = None)

case class BoxToken(accessToken:String, refreshToken:String)

class BoxClient(config:BoxAppConfig){
  val boxAuthenticator = new BoxAuthenticator(config)

  def getUser(boxContext:BoxContext, id:String) = {
    get(boxContext, BoxUserResource(id)).map((new Json2BoxUser).toEntity)
  }

  def getFolder(boxContext:BoxContext, id:String)= {
    get(boxContext, BoxFolderResource(id)).map((new Json2BoxFolder).toEntity)
  }

  def getFolderItems(boxContext:BoxContext, id:String) = {
    get(boxContext, BoxFolderItemsResource(id)).map(new Json2BoxFolderItems(id).toEntity)
  }

  def folderIdByName(boxContext:BoxContext, name:String) = {
    getFolderItems(boxContext, boxContext.getCurrentFolder).map{folderItems=>
      folderItems.items.find{item=>
        item.name == name && item.itemType == BoxItemType.Folder
      }.get
    }
  }

  private def get[T <: BoxEntity](boxContext:BoxContext, resource: BoxResource[T]): Future[JsValue] = {
    val fullUrl = apiUrl + resource.path
    boxContext.tokenFuture flatMap {token =>
      BoxClient.logger.info(s"Sending request to $fullUrl")
      val getFuture = WS.url(fullUrl).withHeaders(getAuthHeader(token)).withQueryString(resource.getParams: _*).get()
      getFuture flatMap {
        response => {
          response.status match {
            case 200 =>
              BoxClient.logger.info(s"Request $fullUrl is completed")
              Future.successful(response.json)
            case _ => Future.failed(BoxHttpErrorException(response))
          }
        }
      }
    }
  }

  private def apiUrl:   String = config.url + "/2.0"
  private def getAuthHeader(token:BoxToken) = "Authorization" -> s"Bearer ${token.accessToken}"
}

object BoxClient {
  def apply(config:BoxAppConfig) = new BoxClient(config) : BoxClient
  val logger = LoggerFactory.getLogger("BoxClient")
}
