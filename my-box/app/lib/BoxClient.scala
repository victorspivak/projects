package lib

import play.api.libs.ws._
import scala.concurrent.Future
import model._
import model.BoxFolderResource
import play.api.libs.json.JsValue

case class BoxAppConfig(url:String, clientId:String, clientSecret:String)
case class BoxToken(accessToken:String, refreshToken:String)

class BoxClient(config:BoxAppConfig, tokenOpt:Option[BoxToken]) {
  implicit val context = scala.concurrent.ExecutionContext.Implicits.global
  val token = tokenOpt match {
    case Some(t) => Future(t)
    case None =>   BoxAuthenticator(config).authenticate("vic+admin@box.com", "test1234")
  }

  def getUser(id:String) = {
    get(BoxUserResource(id)) map (new Json2BoxUser).toEntity
  }

  def getFolder(id:String)= {
    get(BoxFolderResource(id)) map (new Json2BoxFolder).toEntity
  }

  def getFolderItems(id:String) = {
    get(BoxFolderItemsResource(id)) map new Json2BoxFolderItems(id).toEntity
  }

  private def get[T <: BoxEntity](resource: BoxResource[T]): Future[JsValue] = {
    val fullUrl = apiUrl + resource.path
    token flatMap {token =>
      System.out.println(s"Sending request to $fullUrl")
      val getFuture = WS.url(fullUrl).withHeaders(getAuthHeader(token)).withQueryString(resource.getParams: _*).get()
      getFuture map {
        response => {
          response.status match {
            case 200 =>
              System.out.println(s"Request $fullUrl is completed")
              response.json
            case _ => throw new Exception(s"${response.status} ${response.statusText} for ${response.getAHCResponse.getUri.toString}")
          }
        }
      }
    }
  }

  private def apiUrl:   String = config.url + "/2.0"
  private def getAuthHeader(token:BoxToken) = "Authorization" -> s"Bearer ${token.accessToken}"
}

object BoxClient {
  def apply(config:BoxAppConfig, tokenOpt:Option[BoxToken]) = new BoxClient(config, tokenOpt)
}
