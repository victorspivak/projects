package svl.box

import scala.concurrent.Future
import java.lang.Exception
import scala.Exception

class BoxClient {

  val tokenRoot = "https://api.feature01.inside-box.net/api"
  val apiRoot = "https://api.feature01.inside-box.net/api/2.0"

  var accessToken: String = null
  var refreshToken: String = null

  implicit val context = scala.concurrent.ExecutionContext.Implicits.global

  def authenticate(): Future[(String, String)] = {
    val fullUrl = tokenRoot + "/oauth2/token"
    val formData = Map(
      "grant_type" -> Seq("password"),
      "username" -> Seq("mwiller+dev2@box.com"),
      "password" -> Seq("test1234"),
      "client_id" -> Seq("a3nhdenqgvp4q7b2ujj12nu8te0sbsma"),
      "client_secret" -> Seq("4tgqXEPrtohvakK3jv2LD2rJWtFaiGZx")
    )

    val authResultFuture = WS.url(fullUrl).post(formData)
    authResultFuture onFailure {
      case e => {
        System.out.println("HI!! " + e.getMessage)
        throw e
      }
    }

    // Return a new future that returns only auth tokens.
    authResultFuture map {
      case response => {
        accessToken = (response.json \ "access_token").as[String]
        refreshToken = (response.json \ "refresh_token").as[String]
        System.out.println(s"access_token: $accessToken refresh_token: $refreshToken")
        (accessToken, refreshToken)
      }
    }

  }
    /*

    val authTokensFuture = authResultFuture map {
      case response => {
        accessToken = (response.json \ "accessToken").as[String]
        refreshToken = (response.json \ "refreshToken").as[String]
        (accessToken, refreshToken)
      }
    }
    authTokensFuture
  */

  def get(resource: String): Future[JsValue] = {
    System.out.println(s"Called get")
    if(accessToken == null) {
      throw new Exception("Access token not defined")
    }
    val fullUrl = apiRoot + resource
    val getFuture = WS.url(fullUrl).withHeaders("Authorization" -> s"Bearer $accessToken").get()
    System.out.println(s"Sending request to $fullUrl")
    getFuture map {
      response => {
        response.status match {
          case 200 => response.json
          case _ => throw new Exception(s"${response.status} ${response.statusText}")
        }
      }
    }
  }

}
