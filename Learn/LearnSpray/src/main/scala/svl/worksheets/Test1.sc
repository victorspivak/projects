import spray.httpx.marshalling._
import spray.http._
import HttpCharsets._
import MediaTypes._
import spray.http.{MediaTypes, HttpEntity, ContentType}
import spray.json._

import spray.http.{MediaTypes, HttpEntity, ContentType}
import spray.json._

import spray.json.DefaultJsonProtocol


import MediaTypes._
import spray.httpx.unmarshalling._
import svl.learnspray.entities.Person


val source = """{ "some": "JSON source" }"""
val jsonAst = source.parseJson
val json = jsonAst.prettyPrint
val p = Person("Vic", "Spivak", 100)
object MyJsonProtocol extends DefaultJsonProtocol {
  implicit val PersonFormat = jsonFormat3(Person)
}

import MyJsonProtocol._
import spray.httpx.SprayJsonSupport._
import spray.util._
val bob = Person("Bob", "Parr", 32)
val json1 = marshal(bob).right.get.asString

val body = HttpEntity(
  contentType = ContentType(`application/json`, `UTF-8`),
  string = json1
)
val p1 = body.as[Person].right.get
p1.toString
//val p2 = json1.convertTo[Person]
