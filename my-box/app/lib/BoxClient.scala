package lib

import play.api.libs.ws._
import scala.concurrent.Future
import model._
import model.BoxFolderResource
import play.api.libs.json.JsValue

case class BoxAppConfig(url:String, clientId:String, clientSecret:String)
case class BoxToken(accessToken:String, refreshToken:String)

class BoxClient(config:BoxAppConfig) {
  implicit val context = scala.concurrent.ExecutionContext.Implicits.global

  def authenticate(login:String, password:String): Future[BoxToken] = {
    println(s"Authenticating $login started...")
    val authResultFuture = WS.url(tokenUrl).post(postAuthData(login, password))
    authResultFuture onFailure {
      case e => {
        println("***ERROR*** " + e.getMessage)
        throw e
      }
    }

    authResultFuture map {
      case response => {
        println(s"Authenticating $login is successful.")
        BoxToken((response.json \ "access_token").as[String], (response.json \ "refresh_token").as[String])
      }
    }
  }

  def getUser(id:String)(implicit tokenFuture:Future[BoxToken]) = {
    get(BoxUserResource(id), tokenFuture) map (new Json2BoxUser).toEntity
  }

  def getFolder(id:String)(implicit tokenFuture:Future[BoxToken]) = {
    get(BoxFolderResource(id), tokenFuture) map (new Json2BoxFolder).toEntity
  }

  def getFolderItems(id:String)(implicit tokenFuture:Future[BoxToken]) = {
    get(BoxFolderItemsResource(id), tokenFuture) map new Json2BoxFolderItems(id).toEntity
  }

  private def get[T <: BoxEntity](resource: BoxResource[T], tokenFuture:Future[BoxToken]): Future[JsValue] = {
    val fullUrl = apiUrl + resource.path
    tokenFuture flatMap {token =>
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

  private def tokenUrl: String = config.url + "/oauth2/token"
  private def apiUrl:   String = config.url + "/2.0"
  private def getAuthHeader(token:BoxToken) = "Authorization" -> s"Bearer ${token.accessToken}"

  private def postAuthData(login:String, password:String) = Map(
    "grant_type" -> Seq("password"),
    "username" -> Seq(login),
    "password" -> Seq(password),
    "client_id" -> Seq(config.clientId),
    "client_secret" -> Seq(config.clientSecret)
  )
}

object BoxClient {
  def apply(config:BoxAppConfig) = new BoxClient(config)
}
