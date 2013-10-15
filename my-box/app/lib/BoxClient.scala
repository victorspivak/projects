package lib

import play.api.libs.ws._
import scala.concurrent.Future
import model._
import model.BoxFolderResource
import play.api.libs.json.JsValue
import scala.util.Try
import play.api.mvc.Session

case class BoxAppConfig(url:String, clientId:String, clientSecret:String)
case class BoxToken(accessToken:String, refreshToken:String)

class BoxClient(config:BoxAppConfig, tokenOpt:Option[BoxToken]) {
  implicit val context = scala.concurrent.ExecutionContext.Implicits.global
  val token = tokenOpt match {
    case Some(t) => Future(t)
    case None =>   BoxAuthenticator(config).authenticate("vic+admin@box.com", "test1234")
  }

  def getUser(id:String) = {
    get(BoxUserResource(id)).map(_.map( (new Json2BoxUser).toEntity))
  }

  def getFolder(id:String)= {
    get(BoxFolderResource(id)).map(_.map((new Json2BoxFolder).toEntity))
  }

  def getFolderItems(id:String) = {
    get(BoxFolderItemsResource(id)).map(_.map(new Json2BoxFolderItems(id).toEntity))
  }

  private def get[T <: BoxEntity](resource: BoxResource[T]): Future[Try[JsValue]] = {
    val fullUrl = apiUrl + resource.path
    token flatMap {token =>
      System.out.println(s"Sending request to $fullUrl")
      val getFuture = WS.url(fullUrl).withHeaders(getAuthHeader(token)).withQueryString(resource.getParams: _*).get()
      getFuture map {
        response => {
          response.status match {
            case 200 =>
              System.out.println(s"Request $fullUrl is completed")
              Try(response.json)
            case _ => Try(throw new Exception(s"${response.status} ${response.statusText} for ${response.getAHCResponse.getUri.toString}"))
          }
        }
      }
    }
  }

  private def apiUrl:   String = config.url + "/2.0"
  private def getAuthHeader(token:BoxToken) = "Authorization" -> s"Bearer ${token.accessToken}"
}

object BoxClient {
  def apply(config:BoxAppConfig, tokenOpt:Option[BoxToken]) = new BoxClient(config, tokenOpt) : BoxClient
  def apply(implicit session:Session) : BoxClient = {
  //    val boxConfig = new BoxAppConfig("https://api.feature01.inside-box.net/api", "https://api.feature01.inside-box.net/api/2.0",
  //      "a3nhdenqgvp4q7b2ujj12nu8te0sbsma", "4tgqXEPrtohvakK3jv2LD2rJWtFaiGZx")
  //    val resF = bc.authenticate("mwiller+dev2@box.com", "test1234")

      val boxConfig = new BoxAppConfig("https://vspivak.inside-box.net/api",
        "i8ei0dxlkwu8d3n9036trtt436u9kbsc", "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU")

      val token = for {
        accessToken <- session.get("accessToken")
        refreshToken <- session.get("refreshToken")
      } yield BoxToken(accessToken, refreshToken)

      BoxClient(boxConfig, token)
    }
}
