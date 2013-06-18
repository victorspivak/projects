package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.{FeatureIsNotImplementedException, MdType}
import svl.metadata.poc.md.database.{MdQuery, DbObject, DbSession, Database}
import HBaseRichObjects._
import svl.metadata.poc.md.database.solr.SolrEnv

class HBaseSession(val hbaseEnv:HBaseDatabaseEnv) extends DbSession{
  implicit val hbaseEnv_ = hbaseEnv
  val hbaseHelper = hbaseEnv.hbaseHelper

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

  def query(query:MdQuery):List[DbObject] = {
    val queryStr = hbaseEnv.solrEnv.helper.mdQueryToSolrQuery(query)
    println("Query: " + queryStr)

    throw new FeatureIsNotImplementedException("query")
  }

  def disconnect() {}
}

trait HBaseDatabase extends Database {
  def hbaseEnv:HBaseDatabaseEnv
  def solrEnv:SolrEnv

  def connect = hbaseEnv.sessionFactory.newSession
}

