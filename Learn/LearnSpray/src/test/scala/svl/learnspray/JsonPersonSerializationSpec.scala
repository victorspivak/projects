package svl.learnspray

import org.specs2.mutable.Specification
import svl.learnspray.entities.Person

import spray.json._

import spray.json.DefaultJsonProtocol

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._

class JsonPersonSerializationSpec extends Specification {
  val expectedJson = "{\"firstName\":\"Vic\",\"lastName\":\"Spivak\",\"age\":33}"

  "Json Person Marshaller" should {

    s"return $expectedJson for person object" in {
      val p1 = Person("Vic", "Spivak", 33)
      val personString = p1.toJson.toString()
      personString equals expectedJson
    }

    "and properly unmarshaller the json to the case class" in {
      val p1 = Person("Vic", "Spivak", 33)
      val personString = p1.toJson.toString()
      val personJson = personString.parseJson
      val p2 = personJson.convertTo[Person]
      p1 equals p2
    }
  }
}
