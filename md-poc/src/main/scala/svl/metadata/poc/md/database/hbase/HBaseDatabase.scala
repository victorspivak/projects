package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.MdType
import svl.metadata.poc.md.database.{DbObject, DbSession, Database}
import HBaseRichObjects._

class HBaseSession(val env:HBaseDatabaseEnv) extends DbSession{
  implicit val env_ = env
  val helper = env.helper

  def create(dbObj: DbObject) = {
    val table =  dbObj.mdType.table
    val id = dbObj.makeId
    val objWithId = dbObj.setId(id)
    val put = dbObj.objectToPutForCreate(id)

    table.put(put)
    table.close()

    objWithId
  }

  def update(dbObj: DbObject) = {println("UPDATE");null}

  def delete(id: String) {println("DELETE " + id)}

  def fetch(id: String, mdType:MdType) = {
    val table =  mdType.table
    val get = mdType.makeGet(id)
    val result = table.get(get)

    table.close()

    val values = for {
      attribute <- mdType.attributes
      value <- attribute.getValue(result, mdType)
    } yield attribute.name -> value

    if (values.isEmpty) None else Option(DbObject(id, mdType, values.toMap))
  }

  def disconnect() {}
}

trait HBaseDatabase extends Database {
  def env:HBaseDatabaseEnv

  def connect = env.sessionFactory.newSession
}

