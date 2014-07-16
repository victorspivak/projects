//import spray.json._

import spray.json.DefaultJsonProtocol

//import DefaultJsonProtocol._
val source = """{ "some": "JSON source" }"""
val jsonAst = source.parseJson
val json = jsonAst.prettyPrint

case class Person(firstName:String, lastName:String, age:Int)
val p = Person("Vic", "Spivak", 100)

object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._

p.toJson

