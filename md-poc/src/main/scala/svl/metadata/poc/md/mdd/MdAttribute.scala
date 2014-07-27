package svl.metadata.poc.md.mdd

import java.util.Date

object MdAttrDataTypes{
  sealed case class MdAttrDataType[T](dataType:Class[T]){
    val dataTypeName = dataType.getSimpleName
    def asInstanceOf(value:Any) = value.asInstanceOf[T]
  }

  val StringType = MdAttrDataType(classOf[String])
  val IntegerType = MdAttrDataType(classOf[Int])
  val DoubleType = MdAttrDataType(classOf[Double])
  val DateType = MdAttrDataType(classOf[Date])
  val LongType = MdAttrDataType(classOf[Long])
}

import MdAttrDataTypes._

case class MdAttrIndexPolicy(filterable:Boolean, searchable:Boolean)
case class MdAttrStorePolicy(compressing:Boolean, encrypting:Boolean)

case class MdAttributeRef[T](id:String, name:String, attrType:MdAttrDataType[T])

case class MdAttribute[T](id:String, name:String, attrType:MdAttrDataType[T], size:Int,
                       indexPolicy:MdAttrIndexPolicy, storePolicy:MdAttrStorePolicy) {
  def attrValueToString(value:Any) = value.asInstanceOf[T].toString
  def toRef = MdAttributeRef(id, name, attrType)
}

class MdAttributeBuilder[T] (name:String, attrType:MdAttrDataType[T], size:Int = -1) {
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
    new MdAttributeBuilder[String](name, StringType, size)
  }
}

object IntAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder[Int](name, IntegerType)
  }
}

object DoubleAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder[Double](name, DoubleType)
  }
}

object DateAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder[Date](name, DateType)
  }
}

object LongAttributeBuilder {
  def apply(name:String) = {
    new MdAttributeBuilder[Long](name, LongType)
  }
}

