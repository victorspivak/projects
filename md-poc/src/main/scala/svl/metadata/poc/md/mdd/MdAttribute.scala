package svl.metadata.poc.md.mdd

import java.util.Date

import scala.reflect.ClassTag

object MdAttrDataTypes{

  case class DmTypeId(imp:String) extends AnyVal
  case class DmObjId(imp:String) extends AnyVal
  case class OptimisticLocking(imp:Long) extends AnyVal

  sealed case class MdAttrDataType[T](implicit dataType:ClassTag[T]){
    val dataTypeName = dataType.toString()
    def asInstanceOf(value:Any) = value.asInstanceOf[T]
  }

  val StringType = MdAttrDataType[String]()
  val IntegerType = MdAttrDataType[Int]()
  val DoubleType = MdAttrDataType[Double]()
  val DateType = MdAttrDataType[Date]()
  val LongType = MdAttrDataType[Long]()

  val OptimisticLockingType = MdAttrDataType[OptimisticLocking]()
}

import MdAttrDataTypes._

case class MdAttrIndexPolicy(filterable:Boolean, searchable:Boolean)
case class MdAttrStorePolicy(compressing:Boolean, encrypting:Boolean)

case class MdAttribute[+D, +T](objectType:T, id:String,
                       name:String, attrType:MdAttrDataType[D], size:Int,
                       indexPolicy:MdAttrIndexPolicy, storePolicy:MdAttrStorePolicy) {
  def attrValueToString(value:Any) = value.asInstanceOf[D].toString
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

