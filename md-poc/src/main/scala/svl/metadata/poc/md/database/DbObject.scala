package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd._
import svl.metadata.poc.md.mdd.MdAttribute
import scala.language.existentials
import scala.reflect.Manifest

case class DbObject(id: String, mdType: GenericMdType, values: Map[String, Any]) {
  def getValue[T](attr: MdAttribute[T]): Option[T] = {
    val option: Option[Any] = values.get(attr.name)
    option.map(_.asInstanceOf[T])
  }
  def getValue[T](attr: Option[MdAttribute[T]]) = attr.flatMap((a: MdAttribute[T]) => values.get(a.name).map(_.asInstanceOf[T]))
  def getValue[T](name: String)(clazz: Class[T]) = values.get(name).map(_.asInstanceOf[T])

  def getAllValues:List[(MdAttribute[_], Any)] = values.foldLeft(List[(MdAttribute[_], Any)]()){(list, entry) =>
                                          mdType.getAttributeByName(entry._1) -> entry._2 :: list}
  def setId(id: String) = DbObject(id, mdType, values)
  def optimisticLocking: Option[Long] = getValue(mdType.optimisticLockingAttribute)
}

class DbObjectBuilder(id: String, mdType: GenericMdType, overwriteValues: Boolean = false) {
  var values = scala.collection.mutable.HashMap[String, Any]()

  def add(entry: (String, Any)): DbObjectBuilder = {
    val (name, _) = entry
    if (!mdType.containsAttribute(name))
      throw MddExceptions.unknownAttribute(mdType.name, name)
    if (!overwriteValues && values.contains(name))
      throw MddExceptions.duplicateAttributeValue(name)
    values += entry
    this
  }

  private def addAttributeImpl[T](entry: (MdAttribute[T], T)): DbObjectBuilder = add(entry._1.name -> entry._2)
  def addAttribute[T](entry: (MdAttribute[T], T)): DbObjectBuilder = add(entry._1.name -> entry._2)
  def addAttribute[T](entry: (String, T))(implicit m:Manifest[T]): DbObjectBuilder = {
    val (name, value) = entry
    addAttributeImpl(mdType.getAttributeByNameManifest(name) -> value)
  }

  def build = new DbObject(id, mdType, values.toMap)
}

object DbObjectBuilder {
  def apply(id: String, mdType: GenericMdType) = new DbObjectBuilder(id, mdType)

  def apply(mdType: GenericMdType) = new DbObjectBuilder("", mdType)

  def apply(dbObject: DbObject) = dbObject.values.foldLeft(new DbObjectBuilder(dbObject.id, dbObject.mdType, true)) {
    (builder, entry) => builder.add(entry)
  }
}
