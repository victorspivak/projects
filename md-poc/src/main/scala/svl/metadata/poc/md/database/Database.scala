package svl.metadata.poc.md.database

trait DbSession {
  def disconnect()

  def create(dbObj:DbObject):String
  def update(dbObj:DbObject):String
  def delete(id:String)
  def fetch(id:String):Option[DbObject]
}

trait Database {
  def connect:DbSession
}
