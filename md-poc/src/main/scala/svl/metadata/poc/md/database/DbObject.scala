package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd.MdType
import java.util.Date

class DbObject(val id:String, val mdType:MdType, val values:Map[String, Any]) {
  def getString(name:String) = values.get(name).map(_.asInstanceOf[String])
  def getInt(name:String) = values.get(name).map(_.asInstanceOf[Int])
  def getDouble(name:String) = values.get(name).map(_.asInstanceOf[Double])
  def getDate(name:String) = values.get(name).map(_.asInstanceOf[Date])
}
//class DbObject(val id:String, val mdType:MdType, val values:Map[String, Any]) {
//  def getString(name:String) = values.get(name).map(_.asInstanceOf[String])
//  def getInt(name:String) = values.get(name).map(_.asInstanceOf[Int])
//  def getDouble(name:String) = values.get(name).map(_.asInstanceOf[Double])
//  def getDate(name:String) = values.get(name).map(_.asInstanceOf[Date])
//}

