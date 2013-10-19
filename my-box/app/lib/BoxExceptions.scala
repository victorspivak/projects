package lib

import play.api.libs.ws.Response

case class BoxHttpErrorException(statusCode:Int, statusText:String, uri:String) extends Exception {
  override def getMessage: String = s"${getClass.getName}  HTTP code: $statusCode Status Text: $statusText for $uri"
}
object BoxHttpErrorException {
  def apply(response:Response) =
          new BoxHttpErrorException(response.status, response.statusText, response.getAHCResponse.getUri.toString)
}

class BoxRefreshTokenException(statusCode:Int, statusText:String, uri:String) extends BoxHttpErrorException(statusCode, statusText, uri)
object BoxRefreshTokenException {
  def apply(response:Response) =
          new BoxRefreshTokenException(response.status, response.statusText, response.getAHCResponse.getUri.toString)
}

