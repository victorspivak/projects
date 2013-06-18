package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.{UnexpectedStateException, FeatureIsNotImplementedException, MdAttribute, MdType}
import svl.metadata.poc.md.database.DbObject
import svl.metadata.poc.md.mdd.MdIdGenerationPolicies._
import svl.metadata.poc.md.mdd.MdAttrDataTypes._
import org.apache.hadoop.hbase.client.{Result, Put}

object HBaseRichObjects{
  implicit def mdType2HBaseRichType(mdType:MdType)(implicit hbaseEnv:HBaseDatabaseEnv) = new HBaseRichType(mdType)(hbaseEnv)
  implicit def mdObject2HBaseRichDbObject(dbObject:DbObject)(implicit hbaseEnv:HBaseDatabaseEnv) = new HBaseRichDbObject(dbObject)(hbaseEnv)
  implicit def mdAttribute2HBaseRichAttribute[T](mdAttribute:MdAttribute[T])(implicit hbaseEnv:HBaseDatabaseEnv) = new HBaseRichAttribute[T](mdAttribute)(hbaseEnv)
}

import HBaseRichObjects._

class HBaseRichType(val mdType:MdType)(val hbaseEnv:HBaseDatabaseEnv){
  val helper = hbaseEnv.hbaseHelper

  def tableName = mdType.name
  def fieldFamily = "ff_" + mdType.name
  def table = helper.getTable(tableName, Some(fieldFamily))

  def makeGet(id:String) = helper.makeGet(id)
}

class HBaseRichAttribute[T](val attribute:MdAttribute[T])(val hbaseEnv:HBaseDatabaseEnv){
  implicit val hbaseEnv_ = hbaseEnv
  val helper = hbaseEnv.hbaseHelper

  def fieldName = attribute.name
  def getValue(result:Result, mdType:MdType) = helper.getValue(attribute.attrType, result, mdType.fieldFamily, fieldName)
}

class HBaseRichDbObject(val dbObject:DbObject)(val hbaseEnv:HBaseDatabaseEnv){
  implicit val hbaseEnv_ = hbaseEnv
  val helper = hbaseEnv.hbaseHelper

  def makeId = {
    val mdType = dbObject.mdType
    mdType.idGenerationPolicy.policy match {
      case RandomIdPolicy => hbaseEnv.idFactory.makeRandomId
      case SeqIdPolicy => hbaseEnv.idFactory.makeSeqId(mdType.name, mdType.idGenerationPolicy.idTemplate)
      case SuppliedIdPolicy => throw new FeatureIsNotImplementedException("SuppliedIdPolicy")
    }
  }

  def objectToPutForCreate(id:String) = {
    val mdType = dbObject.mdType

    val put = dbObject.values.foldLeft(helper.makePut(id)){(put, entry) =>
      mdType.getAttributeByName(entry._1) match {
        case Some(attr:MdAttribute[_]) => helper.addToPut(put, mdType.fieldFamily, attr.name, attr.attrType, entry._2)
        case None => throw new UnexpectedStateException("Did not find attribute for specified value")
      }
    }

    addOptimisticLockingIfNeeded(put, dbObject)
  }

  private def addOptimisticLockingIfNeeded(put:Put, dbObject:DbObject) = {
    val mdType = dbObject.mdType
    mdType.optimisticLockingAttribute.map((attr:MdAttribute[_]) => {
      helper.addToPut(put, mdType.fieldFamily, attr.name, LongType, 1L)
      put
    }).getOrElse(put)
  }
}
