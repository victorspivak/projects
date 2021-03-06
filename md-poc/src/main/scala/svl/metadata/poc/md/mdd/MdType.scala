package svl.metadata.poc.md.mdd

import scala.language.existentials
import scala.reflect.Manifest

case class MdTypePolicy(optimisticLocking:Boolean, searchable:Boolean)

object MdIdGenerationPolicies{
  sealed case class MdIdGenerationPolicy(ordenal:Int)

  val RandomIdPolicy = MdIdGenerationPolicy(1)
  val SeqIdPolicy = MdIdGenerationPolicy(2)
  val SuppliedIdPolicy = MdIdGenerationPolicy(3)
}
import MdIdGenerationPolicies._
import MdAttrDataTypes._

sealed case class IdGeneration(idColumn:MdAttribute[_, _], policy:MdIdGenerationPolicy, idTemplate:String)

trait MdType{
  def id:DmTypeId
  def name:String
  def attributes:List[MdAttribute[Any, MdType]]
  def idGeneration:IdGeneration

  val attributesByName:Map[String, MdAttribute[_,_]] =
    attributes.foldLeft(Map[String, MdAttribute[_, _]]()) ((m:Map[String, MdAttribute[_, _]], a:MdAttribute[_, _]) => m + (a.name -> a))

  def getAttributeByNameOpt(attrName:String) = attributesByName.get(attrName)
  def getAttributeByName(attrName:String):MdAttribute[_] = getAttributeByNameOpt(attrName) match {
    case Some(attr) => attr
    case _ => throw MddExceptions.unknownAttribute(name, attrName)
  }
  def getAttributeByNameType[T](attrName:String)(clazz:Class[T]):MdAttribute[T] = getAttributeByName(attrName).asInstanceOf[MdAttribute[T]]

  def getAttributeByNameManifest[T](attrName:String)(implicit m:Manifest[T]):MdAttribute[T] =
    getAttributeByName(attrName).asInstanceOf[MdAttribute[T]]

  def containsAttribute(attrName:String) = attributesByName.contains(attrName)
  def idColumn = idGeneration.idColumn
  def idGenerationPolicy = idGeneration
  def optimisticLockingAttribute:Option[MdAttribute[Long]] = getAttributeByNameOpt(GenericMdType.OptimisticLockingColumnName).asInstanceOf[Option[MdAttribute[Long]]]
}

case class GenericMdType(id:String, name:String, attributes:List[MdAttribute[_]], idGeneration:IdGeneration) extends MdType
//{
//  val attributesByName:Map[String, MdAttribute[_]] =
//    attributes.foldLeft(Map[String, MdAttribute[_]]()) ((m:Map[String, MdAttribute[_]], a:MdAttribute[_]) => m + (a.name -> a))
//
//  def getAttributeByNameOpt(attrName:String) = attributesByName.get(attrName)
//  def getAttributeByName(attrName:String):MdAttribute[_] = getAttributeByNameOpt(attrName) match {
//    case Some(attr) => attr
//    case _ => throw MddExceptions.unknownAttribute(name, attrName)
//  }
//  def getAttributeByNameType[T](attrName:String)(clazz:Class[T]):MdAttribute[T] = getAttributeByName(attrName).asInstanceOf[MdAttribute[T]]
//
//  def getAttributeByNameManifest[T](attrName:String)(implicit m:Manifest[T]):MdAttribute[T] =
//    getAttributeByName(attrName).asInstanceOf[MdAttribute[T]]
//
//  def containsAttribute(attrName:String) = attributesByName.contains(attrName)
//  def idColumn = idGeneration.idColumn
//  def idGenerationPolicy = idGeneration
//  def optimisticLockingAttribute:Option[MdAttribute[Long]] = getAttributeByNameOpt(GenericMdType.OptimisticLockingColumnName).asInstanceOf[Option[MdAttribute[Long]]]
//}

object GenericMdType {
  val OptimisticLockingColumnName = "OptLockingAttr"
}

class MdTypeBuilder (val name:String) {
  var attributes = scala.collection.mutable.ArrayBuffer[MdAttribute[_]]()
  var attributesIndex = scala.collection.mutable.HashMap[String, MdAttribute[_]]()
  var optimisticLocking = false
  var idColumn:MdAttribute[_] = null
  var idTemplate = MdTypeBuilder.DefaultIdTemplate
  var idGenerationPolicy:MdIdGenerationPolicy = MdTypeBuilder.DefaultIdPolicy

  def add (attribute:MdAttribute[_]):MdTypeBuilder = {
    if (attributesIndex.contains(attribute.name))
      throw MddExceptions.duplicateAttribute(name, attribute.name)
    if (attribute.name == GenericMdType.OptimisticLockingColumnName)
      throw MddExceptions.invalidAttributeName(name, attribute.name)
    attributes += attribute
    attributesIndex += attribute.name -> attribute
    this
  }

  def id (attribute:MdAttribute[_]):MdTypeBuilder = {
    if (idColumn != null)
      throw MddExceptions.duplicateIdAttribute(name, attribute.name, idColumn.name)
    this add attribute
    idColumn = attribute
    this
  }

  def doOptimisticLocking() = {
    this.optimisticLocking = true
    this
  }

  def use(idGenerationPolicy:MdIdGenerationPolicy, idTemplate:String = MdTypeBuilder.DefaultIdTemplate) = {
    this.idGenerationPolicy = idGenerationPolicy
    this.idTemplate = idTemplate
    this
  }

  def build = {
    def addOptimisticAttrIfNeeded(): List[MdAttribute[_]] = {
      if (optimisticLocking)
        LongAttributeBuilder(GenericMdType.OptimisticLockingColumnName).build :: attributes.toList
      else
        attributes.toList
    }

    if (idColumn == null)
      throw MddExceptions.missingIdColumn(name)

    new GenericMdType("", name, addOptimisticAttrIfNeeded(), IdGeneration(idColumn, idGenerationPolicy, idTemplate))
  }
}

object MdTypeBuilder {
  import scala.language.implicitConversions
  val DefaultIdPolicy = SeqIdPolicy
  val DefaultIdTemplate = "id_%09d"
  implicit def mdAttributeBuilder2MdAttribute(builder:MdAttributeBuilder[_]) = builder.build
  def apply (name:String) = new MdTypeBuilder(name)
}