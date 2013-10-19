package lib

import play.api.libs.ws.Response

//response.status} ${response.statusText} for ${response.getAHCResponse.getUri.toString}
class BoxHttpErrorException(statusCode:Int, statusText:String, uri:String) extends Exception {
  override def getMessage: String = s"${getClass.getName}  HTTP code: $statusCode Status Text: $statusText for $uri"
}

object BoxHttpErrorException {
  def apply(response:Response) =
          new BoxHttpErrorException(response.status, response.statusText, response.getAHCResponse.getUri.toString)
}
class BoxAuthenticationException(cause:Exception = null) extends Exception(cause)
