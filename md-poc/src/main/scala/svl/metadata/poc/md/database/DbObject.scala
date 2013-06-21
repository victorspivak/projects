package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd.{IdGeneration, MddExceptions, MdAttribute, MdType}
import java.util.Date

case class DbObject(id:String, mdType:MdType, values:Map[String, Any]) {
  def getValue[T](attr:MdAttribute[T]):Option[T] = values.get(attr.name).map(_.asInstanceOf[T])
  def getValue[T](attr:Option[MdAttribute[T]]) = attr.flatMap((a:MdAttribute[T]) => values.get(a.name).map(_.asInstanceOf[T]))
  def getValue[T](name:String)(clazz:Class[T]) = values.get(name).map(_.asInstanceOf[T])

  def setId(id:String) = DbObject(id, mdType, values)
  def optimisticLocking:Option[Long] = getValue(mdType.optimisticLockingAttribute)
}

class DbObjectBuilder (id:String, mdType:MdType) {
  var values = scala.collection.mutable.HashMap[String, Any]()

  def add (entry:(String, Any)):DbObjectBuilder = {
    val (name, value) = entry
    if (!mdType.containsAttribute(name))
      throw MddExceptions.unknownAttribute(mdType.name, name)
    if (values.contains(name))
      throw MddExceptions.duplicateAttributeValue(name)
    values += entry
    this
  }

  def addAttribute[T] (entry:(MdAttribute[T], T)):DbObjectBuilder = add(entry._1.name -> entry._2)

  def build = new DbObject(id, mdType, values.toMap)
}

object DbObjectBuilder {
  def apply (id:String, mdType:MdType) = new DbObjectBuilder(id, mdType)
  def apply (mdType:MdType) = new DbObjectBuilder("", mdType)
}
