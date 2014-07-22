package svl.learn.scala.json

import spray.json._

object SprayCustomExample extends App{
  class Color(val name: String, val red: Int, val green: Int, val blue: Int)

  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit object ColorJsonFormat extends RootJsonFormat[Color] {
      //    def write(c: Color) =
      //      JsArray(JsString(c.name), JsNumber(c.red), JsNumber(c.green), JsNumber(c.blue))
      //
      //    def read(value: JsValue) = value match {
      //      case JsArray(JsString(name) :: JsNumber(red) :: JsNumber(green) :: JsNumber(blue) :: Nil) =>
      //        new Color(name, red.toInt, green.toInt, blue.toInt)
      //      case _ => deserializationError("Color expected")
      //    }
      //
      def write(c: Color) = JsObject(
        "my-name" ->JsString(c.name),
        "my-red" -> JsNumber(c.red),
        "my-green" -> JsNumber(c.green),
        "my-blue" -> JsNumber(c.blue))

      def read(value:JsValue) = {
        value.asJsObject.getFields("my-name", "my-red", "my-green", "my-blue") match {
          case Seq(JsString(name), JsNumber(red), JsNumber(green), JsNumber(blue)) =>
            new Color(name, red.toInt, green.toInt, blue.toInt)
          case _ => throw new Exception("Oops")
        }
      }
    }
  }

  import MyJsonProtocol._

  val json = new Color("CadetBlue", 95, 158, 160).toJson
  println(json.toString())
  val color = json.convertTo[Color]
  println(color)
}
