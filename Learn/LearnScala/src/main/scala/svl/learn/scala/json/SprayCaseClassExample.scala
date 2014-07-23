package svl.learn.scala.json

import spray.json._

object SprayCaseClassExample extends App {

  case class Color(name: String, red: Int, green: Int, blue: Int)


  case class NamedList[A](name: String, items: List[A])

  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit val ColorJsonFormat = jsonFormat4(Color)
    implicit def namedListFormat[A :JsonFormat] = jsonFormat2(NamedList.apply[A])
  }

  import MyJsonProtocol._

  val json1 = new Color("CadetBlue", 95, 158, 160).toJson
  println(json1.toString())
  val color = json1.convertTo[Color]
  println(color)
  val json2 = NamedList("items", List(1,2,3)).toJson
  println(json2)
}
