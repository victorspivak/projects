package svl.metadata.web

import org.scalatra._
import org.json4s.{JsonDSL, DefaultFormats, Formats}
import org.scalatra.json._
import JsonDSL._

//class MyScalatraServlet extends MyScalatraWebAppStack {
class MyScalatraServlet extends ScalatraServlet with JacksonJsonSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

//  before() {
//    contentType = formats("json")
//  }
  get("/") {
    <html>
      <body>
        <h1>Hello, Scalatra!</h1>
      </body>
    </html>
  }

  get("/flowers") {
    ("db name" -> "db") ~
    ("collection mame" -> "coll name") ~
    ("count" -> 100) ~
    ("timing" -> 1000)
  }
}

case class Flower(slug: String, name: String)
object FlowerData {
  var all = List(
    Flower("yellow-tulip", "Yellow Tulip"),
    Flower("red-rose", "Red Rose"),
    Flower("black-rose", "Black Rose"))
}