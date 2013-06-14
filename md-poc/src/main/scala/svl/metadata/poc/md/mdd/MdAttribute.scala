package svl.metadata.poc.md.mdd

import java.util.Date

object MdAttrDataTypes{
  sealed trait MdAttrDataType[T]{}
  sealed case class StringTypeImp() extends MdAttrDataType[String]
  sealed case class IntTypeImp() extends MdAttrDataType[Int]
  sealed case class DoubleTypeImp() extends MdAttrDataType[Double]
  sealed case class DateTypeImp() extends MdAttrDataType[Date]
  sealed case class LongTypeImp() extends MdAttrDataType[Long]

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

class MdAttributeBuilder (name:String, attrType:MdAttrDataType[_], size:Int = -1) {
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
    new MdAttributeBuilder(name, StringType, size)
  }
}

object IntAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder(name, IntegerType)
  }
}

object DoubleAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder(name, DoubleType)
  }
}

object DateAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder(name, DateType)
  }
}

object LongAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder(name, LongType)
  }
}

