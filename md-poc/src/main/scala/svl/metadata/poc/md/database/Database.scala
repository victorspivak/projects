package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd.MdType

trait DbBatchOperations {
  def create(dbObjects:List[DbObject], mdType:MdType):List[DbObject]
  def delete(id:String)
  def fetch(ids:List[String], mdType:MdType):List[DbObject]
}

trait SingleObjectOperations {
  def create(dbObj:DbObject):DbObject
  def update(dbObj:DbObject)
  def delete(id:String)
  def fetch(id:String, mdType:MdType):Option[DbObject]
  def fetch(dbObj:DbObject):Option[DbObject]
  def query(query:MdQuery):List[DbObject]
}

trait DbSession extends SingleObjectOperations with DbBatchOperations{
  def disconnect()
}

trait Database {
  def connect:DbSession
}
