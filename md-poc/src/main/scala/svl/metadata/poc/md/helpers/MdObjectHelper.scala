package svl.metadata.poc.md.helpers

import svl.metadata.poc.md.database.DbObject
import org.json4s.{JsonAST, JsonDSL, DefaultFormats, Formats}
import JsonDSL._
import scala.language.existentials

object MdObjectHelper {

  def dump(obj:DbObject) {
    val str = dbObjectToString(obj)
    println(str)
  }

  def dbObjectToString(obj:DbObject) = {
    def formatAttribute(name:String, value:String) = "%s => \'%s\'".format(name, value)
    val mdType = obj.mdType
    val id = obj.id
    val attrs = obj.getAllValues
    val values = attrs.map{entry => val (attr, value) = entry
      formatAttribute(attr.name, attr.attrValueToString(value))
    }

    val res = values.mkString("Obj: " + formatAttribute(mdType.idColumn.name, id) + "   ", "  ", "")
    res
  }

  def dbObjectToJson(obj:Option[DbObject]):Option[JsonAST.JObject] = {
    obj.map(dbObjectToJson)
  }

  def dbObjectToJson(obj:DbObject):JsonAST.JObject = {
    val mdType = obj.mdType
    val id = obj.id
    val attrs = obj.getAllValues
    val values = attrs.map{entry => val (attr, value) = entry
      attr.name -> attr.attrValueToString(value)
    }
    values.foldLeft(JsonAST.JObject() ~ (mdType.idColumn.name -> id)) (_~_)
  }
}
