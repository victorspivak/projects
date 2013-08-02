//package svl.mongo
//
//import com.mongodb.casbah.{MongoCollection, MongoClient}
//import com.mongodb.casbah.commons.MongoDBObject
//import scala.collection.mutable
//import com.mongodb.{WriteConcern, DBObject}
//import com.mongodb.casbah.Imports._
//
//class Db (val host:String, val dbName:String) {
//  val mongoClient =  MongoClient(host)
//  val mongoDb = mongoClient(dbName)
//
//  def deleteCollection(name:String) = collection(name).remove(MongoDBObject.empty)
//  def collection(name:String): MongoCollection = mongoDb(name)
//}
//
//object Db {
//  def apply(host:String, dbName:String) = new Db(host, dbName)
//}
//
//trait AbstractEntity {
//  def toDbObject:DBObject
//}
//
//class AbstractFilter {
//  val constrains = new mutable.MutableList[(String, Any)]
//  def filterEquals(key:String, value:Any) = add(key -> value)
//  def filterGT(key:String, value:Any) = add(key -> MongoDBObject("$gt" -> value))
//  def filterLT(key:String, value:Any) = add(key -> MongoDBObject("$lt" -> value))
//
//  private def add(constrain:(String, Any)) = {
//    constrains += constrain
//    this
//  }
//
//  def toDbObject:DBObject = MongoDBObject(constrains.toList)
//}
//
//object AbstractFilter {
//  def apply() = new AbstractFilter()
//}
//
//abstract class EntityActor[T <: AbstractEntity](val collectionName:String) (implicit val db:Db) {
//  def convert(dbObj:DBObject):T
//
//  val collection = db.collection(collectionName)
//  implicit def toDbObject(t:T):DBObject = t.toDbObject
//  def insert(obj:T) = collection.insert(obj.toDbObject, WriteConcern.SAFE)
//  def insert(objects:T*) = {
//    collection.insert(objects.map(_.toDbObject): _*)
//    collection.lastError(WriteConcern.SAFE)
//  }
////  def insert(objects:T*) = collection.insert(objects)
////  def insert(o1:T, o2:T) = collection.insert(List(o1, o2): _*)
//
//  def query(filter:AbstractFilter) = collection.find(filter.toDbObject).map(convert(_))
//  def count(filter:AbstractFilter) = collection.count(filter.toDbObject)
//}
