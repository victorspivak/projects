package svl.metadata.poc.md.database

import svl.metadata.poc.md.mdd.MdType

trait DbSession {
  def disconnect()

  def create(dbObj:DbObject):DbObject
  def update(dbObj:DbObject):String
  def delete(id:String)
  def fetch(id:String, mdType:MdType):Option[DbObject]
  def query(query:MdQuery):List[DbObject]
}

trait Database {
  def connect:DbSession
}
