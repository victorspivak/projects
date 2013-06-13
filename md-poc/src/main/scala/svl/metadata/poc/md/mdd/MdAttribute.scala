package svl.metadata.poc.md.mdd

import java.util.Date

object MdAttrDataTypes{
  trait MdAttrDataType[T]{}
  case class StringTypeImp() extends MdAttrDataType[String]
  case class IntTypeImp() extends MdAttrDataType[Int]
  case class DoubleTypeImp() extends MdAttrDataType[Double]
  case class DateTypeImp() extends MdAttrDataType[Date]
  case class LongTypeImp() extends MdAttrDataType[Long]

  val StringType = StringTypeImp()
  val IntegerType = IntTypeImp()
  val DoubleType = DoubleTypeImp()
  val DateType = DateTypeImp()
  val LongType = LongTypeImp()
}

import MdAttrDataTypes._

case class MdAttrIndexPolicy(filterable:Boolean, searchable:Boolean)
case class MdAttrStorePolicy(compressing:Boolean, encrypting:Boolean)

case class MdAttribute(id:String, name:String, attrType:MdAttrDataType[_], size:Int,
                       indexPolicy:MdAttrIndexPolicy, storePolicy:MdAttrStorePolicy) {
}

class AttributeBuilder (name:String, attrType:MdAttrDataType[_], size:Int = -1) {
  var filterable = false
  var searchable = false
  var compressing = false
  var encrypting = false

  def build = {
    new MdAttribute("", name, attrType, size, new MdAttrIndexPolicy(filterable, searchable), new MdAttrStorePolicy(compressing, encrypting))
  }

  def doFiltering() = {
    this.filterable = true
    this
  }

  def doSearching() = {
    this.searchable = true
    this
  }

  def doCompressing() = {
    this.compressing = true
    this
  }

  def doEncrypting() = {
    this.encrypting = true
    this
  }
}
object StringAttributeBuilder {
  def apply(name:String, size:Int = -1) = {
    new AttributeBuilder(name, StringType, size)
  }
}

object IntAttributeBuilder {
  def apply(name:String) = {
    new AttributeBuilder(name, IntegerType)
  }
}

object DoubleAttributeBuilder {
  def apply(name:String) = {
    new AttributeBuilder(name, DoubleType)
  }
}

object DateAttributeBuilder {
  def apply(name:String) = {
    new AttributeBuilder(name, DateType)
  }
}

object LongAttributeBuilder {
  def apply(name:String) = {
    new AttributeBuilder(name, LongType)
  }
}

