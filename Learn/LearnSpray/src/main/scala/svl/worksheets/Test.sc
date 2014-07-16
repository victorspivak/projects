import svl.learnspray.entities.Person

import spray.http.MediaTypes
import spray.json._

import spray.json.DefaultJsonProtocol
val p = Person("Vic", "Spivak", 100)
object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._

val personJson = p.toJson

//val body = HttpEntity(
//  contentType = ContentType(`application/json`),
//  string = personJson
//)




