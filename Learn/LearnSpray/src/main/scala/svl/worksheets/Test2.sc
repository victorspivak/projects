import svl.learnspray.entities._
import spray.json._
import MyJsonProtocol._


val user1 = User("Vic", "Spivak", company = Some("Box"))
val user1String = user1.toJson.toString()
val user1Json = user1String.parseJson
val user2 = user1Json.convertTo[User]



