package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.{UnexpectedStateException, FeatureIsNotImplementedException, MdAttribute, MdType}
import svl.metadata.poc.md.database.DbObject
import svl.metadata.poc.md.mdd.MdIdGenerationPolicies._
import svl.metadata.poc.md.mdd.MdAttrDataTypes._
import org.apache.hadoop.hbase.client.{Result, Put}

object HBaseRichObjects{
  implicit def mdType2HBaseRichType(mdType:MdType)(implicit env:HBaseDatabaseEnv) = new HBaseRichType(mdType)(env)
  implicit def mdObject2HBaseRichDbObject(dbObject:DbObject)(implicit env:HBaseDatabaseEnv) = new HBaseRichDbObject(dbObject)(env)
  implicit def mdAttribute2HBaseRichAttribute[T](mdAttribute:MdAttribute[T])(implicit env:HBaseDatabaseEnv) = new HBaseRichAttribute[T](mdAttribute)(env)
}

import HBaseRichObjects._

class HBaseRichType(val mdType:MdType)(val env:HBaseDatabaseEnv){
  val helper = env.helper

  def tableName = mdType.name
  def fieldFamily = "ff_" + mdType.name
  def table = helper.getTable(tableName, Some(fieldFamily))

  def makeGet(id:String) = helper.makeGet(id)
}

class HBaseRichAttribute[T](val attribute:MdAttribute[T])(val env:HBaseDatabaseEnv){
  implicit val env_ = env
  val helper = env.helper

  def fieldName = attribute.name
  def getValue[T](result:Result, mdType:MdType) = helper.getValue(attribute.attrType, result, mdType.fieldFamily, fieldName)
}

class HBaseRichDbObject(val dbObject:DbObject)(val env:HBaseDatabaseEnv){
  implicit val env_ = env
  val helper = env.helper

  def makeId = {
    val mdType = dbObject.mdType
    mdType.idGenerationPolicy.policy match {
      case RandomIdPolicy => env.idFactory.makeRandomId
      case SeqIdPolicy => env.idFactory.makeSeqId(mdType.name, mdType.idGenerationPolicy.idTemplate)
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


