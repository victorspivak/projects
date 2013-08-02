package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.{MddExceptions, MdAttrDataTypes, MdType}
import svl.metadata.poc.md.database.{MdQuery, DbObject, DbSession, Database}
import HBaseRichObjects._
import svl.metadata.poc.md.database.solr.SolrEnv
import org.apache.hadoop.hbase.client.{Put, Get, Result}
import org.apache.hadoop.hbase.util.Bytes

class HBaseSession(val context:HBaseDatabaseContext) extends DbSession{
  implicit val context_ = context
  val hbaseHelper = context.hbaseHelper

  def create(dbObj: DbObject) = {
    val table =  dbObj.mdType.table
    val objWithId = dbObj.assignId
    val put = objWithId.objectToPutForCreate

    table.put(put)
    context.solrEnv.helper.indexDocument(objWithId)

    context.solrEnv.helper.indexDocument(objWithId)

    objWithId
  }

  def update(dbObj: DbObject) {
    import scala.language.implicitConversions
    implicit def string2Bytes(value:String) = Bytes.toBytes(value)
    import MdAttrDataTypes._

    val mdType = dbObj.mdType
    val table =  mdType.table

    val put = dbObj.objectToPutForUpdate

    dbObj.optimisticLocking match {
      case Some(optLock) =>
        if (!table.checkAndPut(dbObj.id, mdType.fieldFamily, MdType.OptimisticLockingColumnName, hbaseHelper.toBytes(LongType, optLock), put))
          throw MddExceptions.concurrentObjectUpdate(dbObj)
      case None => table.put(put)
    }

    context.solrEnv.helper.indexDocument(dbObj)
  }

  def delete(id: String) {println("DELETE " + id)}

  def fetch(dbObj:DbObject):Option[DbObject] = fetch(dbObj.id, dbObj.mdType)
  def fetch(id: String, mdType:MdType) = {
    val table =  mdType.table
    val get = mdType.makeGet(id)
    val result = table.get(get)

    getResultToDbObject(result, id, mdType)
  }

  def getResultToDbObject(result: Result, id: String, mdType: MdType): Option[DbObject] = {
    val values = for {
      attribute <- mdType.attributes
      value <- attribute.getValue(result, mdType)
    } yield attribute.name -> value

    if (values.isEmpty) None else Option(DbObject(id, mdType, values.toMap))
  }

  def query(query:MdQuery):List[DbObject] = {
    val ids = context.solrEnv.helper.query(query).map(_.id)
    fetch(ids, query.mdType)
  }

  def disconnect() {}

  def create(dbObjects: List[DbObject], mdType:MdType) = {
    import scala.collection.JavaConversions._
    val table =  mdType.table
    val objWithIds = dbObjects.map(_.assignId)
    val puts = objWithIds.foldLeft(List[Put]()){(list, obj) =>
      obj.objectToPutForCreate :: list
    }

    table.batch(puts) //svl handle errors

    context.solrEnv.helper.indexDocument(objWithIds, mdType)

    objWithIds
  }

  def fetch(ids: List[String], mdType:MdType) = {
    import scala.collection.JavaConversions._
    val table =  mdType.table
    val gets = ids.foldLeft(List[Get]()){(gets, id) =>
      mdType.makeGet(id) :: gets
    }

    val result = table.batch(gets)
    result.foldLeft(List[DbObject]()){(list, res) =>
      val getResult:Result = res.asInstanceOf[Result]
      getResultToDbObject(getResult, Bytes.toString(getResult.getRow), mdType) match {
        case Some(obj) => obj :: list
        case None => list
      }
    }.reverse
  }
}

trait HBaseDatabase extends Database {
  def context:HBaseDatabaseContext
  def solrEnv:SolrEnv

  def connect = context.sessionFactory.newSession
}
