package svl.learnspray

import org.specs2._

import svl.learnspray.entities.Person

import spray.json._
import MyJsonProtocol._

class JsonPersonAcceptanceSpec extends Specification {
  def is = s2"""

 This is a specification to check Person Json Marshaller

 The Person marshaller should
   convert a person object into json string                               $e1
   and then it should be able to convert this string back to the object   $e2
   and this object should not be the same reference                       $e3
                                                                 """
  val expectedJson = "{\"firstName\":\"Vic\",\"lastName\":\"Spivak\",\"age\":33}"
  val p1 = Person("Vic", "Spivak", 33)
  val personString = p1.toJson.toString()
  val personJson = personString.parseJson
  val p2 = personJson.convertTo[Person]

  def e1 = personString mustEqual expectedJson
  def e2 = p2 mustEqual p1
  def e3 = p2 should not be p1
}
