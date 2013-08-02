package svl.metadata.web

import org.scalatra._
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json._
import svl.metadata.poc.md.dictionary.MdTypes

class TypesRestServlet extends ScalatraServlet with JacksonJsonSupport {
  protected implicit val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
  }

  get("/*") {
    val paths = requestPath.substring(1).split("/")

    paths.length match {
      case 1 => MdTypes.getType(paths(0)).getOrElse(NotFound(s"Type \'${paths(0)}\' not found"))
    }
  }
}
