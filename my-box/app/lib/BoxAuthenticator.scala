package lib

import play.api.mvc._
import scala.concurrent.Future
import play.api.libs.ws.WS
import play.api.mvc.Results.Redirect
import scala.concurrent.ExecutionContext.Implicits.global

class BoxAuthenticator(config:BoxAppConfig) {
  def getOauth2Code(implicit request:Request[AnyContent]) = Redirect(authorizeUrl, postAuthDataForCode)

  def authenticate(code:String): Future[BoxToken] = {
    println(s"Authenticating $code started...")
    WS.url(tokenUrl).post(postAuthDataForOauthTokens(code)) flatMap {
      case response if response.status == 200 =>
        println(s"Authenticating $code is successful.")
        val token: BoxToken = BoxToken((response.json \ "access_token").as[String], (response.json \ "refresh_token").as[String])
        Future.successful(token)
      case response =>
        println(">>>>>>>>>>>>>>>>>>>>>>>> " + response.json)
        Future.failed(BoxHttpErrorException(response))
    }
  }

  def refresh(token:BoxToken): Future[BoxToken] = {
    println(s"Refresh token started...")
    WS.url(tokenUrl).post(postAuthDataForRefresh(token.refreshToken)) flatMap {
      case response if response.status == 200 =>
        println(s"Refresh token is successful.")
        val token: BoxToken = BoxToken((response.json \ "access_token").as[String], (response.json \ "refresh_token").as[String])
        Future.successful(token)
      case response =>
        println(">>>>>>>>>>>>>>>>>>>>>>>> " + response.json)
        Future.failed(BoxRefreshTokenException(response))
    }
  }

  def authenticate(login:String, password:String): Future[BoxToken] = {
    println(s"Authenticating $login started...")
    WS.url(tokenUrl).post(postAuthDataForPassword(login, password)) flatMap {
      case response if response.status == 200 =>
        println(s"Authenticating $login is successful.")
        Future.successful(BoxToken((response.json \ "access_token").as[String], (response.json \ "refresh_token").as[String]))
      case response =>
        Future.failed(BoxHttpErrorException(response))
    }
  }

  private def tokenUrl: String = config.url + "/oauth2/token"
  private def authorizeUrl: String = config.url + "/oauth2/authorize"

  private def postAuthDataForCode(implicit request:Request[AnyContent]) = Map(
    "response_type" -> Seq("code"),
    "redirect_uri" -> Seq(config.redirectUrl.getOrElse("controllers.routes.Application.authtoken().absoluteURL(secure = true)")),
    "client_id" -> Seq(config.clientId)
  )

  private def postAuthDataForOauthTokens(code:String) = Map(
    "client_id" -> Seq(config.clientId),
    "client_secret" -> Seq(config.clientSecret),
    "grant_type" -> Seq("authorization_code"),
    "code" -> Seq(code)
  )

  private def postAuthDataForRefresh(refresh_token:String) = Map(
    "client_id" -> Seq(config.clientId),
    "client_secret" -> Seq(config.clientSecret),
    "grant_type" -> Seq("refresh_token"),
    "refresh_token" -> Seq(refresh_token)
  )

  private def postAuthDataForPassword(login:String, password:String) = Map(
    "grant_type" -> Seq("password"),
    "username" -> Seq(login),
    "password" -> Seq(password),
    "client_id" -> Seq(config.clientId),
    "client_secret" -> Seq(config.clientSecret)
  )
}
