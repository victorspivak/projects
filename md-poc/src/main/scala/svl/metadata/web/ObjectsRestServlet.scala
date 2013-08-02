package svl.metadata.web

import org.scalatra._
import org.json4s.{JsonDSL, DefaultFormats, Formats}
import org.scalatra.json._
import svl.metadata.poc.md.database.ObjectManager
import svl.metadata.poc.md.helpers.MdObjectHelper

class ObjectsRestServlet extends ScalatraServlet with JacksonJsonSupport {
    protected implicit val jsonFormats: Formats = DefaultFormats

  get("/*") {
    val paths = requestPath.substring(1).split("/")

    paths.length match {
      case 2 => MdObjectHelper.dbObjectToJson(ObjectManager().fetch(paths(0), paths(1))).getOrElse(response.setStatus(403))

    }
  }
}
