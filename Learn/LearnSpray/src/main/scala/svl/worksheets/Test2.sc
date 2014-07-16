import svl.learnspray.entities.Person

import spray.http.MediaTypes
import spray.json._

import spray.json.DefaultJsonProtocol
val p = Person("Vic", "Spivak", 33)
object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._

val personString = p.toJson.toString()
val personJson = personString.parseJson
val p2 = personJson.convertTo[Person]






