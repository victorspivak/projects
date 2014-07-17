package svl.learnspray.entities

import spray.json.DefaultJsonProtocol

case class Person(firstName:String, lastName:String, age:Int)

case class User(firstName:String, lastName:String, email:Option[String] = None, company:Option[String] = None)

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
  implicit val UserFormat = jsonFormat4(User)
}
