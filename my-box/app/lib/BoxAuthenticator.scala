package lib

import scala.concurrent.Future
import play.api.libs.ws.WS

class BoxAuthenticator(config:BoxAppConfig) {
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

  private def tokenUrl: String = config.url + "/oauth2/token"
  private def postAuthData(login:String, password:String) = Map(
    "grant_type" -> Seq("password"),
    "username" -> Seq(login),
    "password" -> Seq(password),
    "client_id" -> Seq(config.clientId),
    "client_secret" -> Seq(config.clientSecret)
  )
}

object BoxAuthenticator {
  def apply(config:BoxAppConfig) = new BoxAuthenticator(config)
}
