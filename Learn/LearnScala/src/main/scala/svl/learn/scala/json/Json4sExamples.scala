package svl.learn.scala.json

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

object Json4sExamples {
  def main(args: Array[String]) {
    case class Person(firstName:String, lastName:String, age:Int)

    val vic = Person("Victor", "Spivak", 33)
    implicit val formats = Serialization.formats(NoTypeHints)
    val personJsonString = Serialization.write(vic)
    println(personJsonString)
    val personJson = parse(personJsonString)
    val person = personJson.extract[Person]
    println(person)

  }
}
