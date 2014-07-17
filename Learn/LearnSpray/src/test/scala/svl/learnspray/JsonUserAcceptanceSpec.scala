package svl.learnspray

import org.specs2._

import svl.learnspray.entities._
import spray.json._
import svl.learnspray.entities.MyJsonProtocol._

class JsonUserAcceptanceSpec extends Specification {
  def is = s2"""

 This is a specification to check User Json Marshaller. It uses optional case class fields.

 The User marshaller should
   convert an user object into json string                                $e1
   and then it should be able to convert this string back to the object   $e2
                                                                 """

  val expectedJson = "{\"firstName\":\"Vic\",\"lastName\":\"Spivak\",\"company\":\"Box\"}"
  val user1 = User("Vic", "Spivak", company = Some("Box"))

  val user1String = user1.toJson.toString()
  val user1Json = user1String.parseJson
  val user2 = user1Json.convertTo[User]

  def e1 = user1String mustEqual expectedJson
  def e2 = user1 mustEqual user2
}
