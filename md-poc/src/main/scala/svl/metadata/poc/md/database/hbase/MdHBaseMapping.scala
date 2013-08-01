package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd._
import svl.metadata.poc.md.mdd.MdIdGenerationPolicies._
import svl.metadata.poc.md.mdd.MdAttrDataTypes._
import org.apache.hadoop.hbase.client.{Result, Put}
import svl.metadata.poc.md.mdd.MdAttribute
import scala.Some
import svl.metadata.poc.md.database.DbObject

object HBaseRichObjects{
  import scala.language.implicitConversions
  implicit def mdType2HBaseRichType(mdType:MdType)(implicit context:HBaseDatabaseContext) =
                                                                        new HBaseRichType(mdType)(context)
  implicit def mdObject2HBaseRichDbObject(dbObject:DbObject)(implicit context:HBaseDatabaseContext) =
                                                                        new HBaseRichDbObject(dbObject)(context)
  implicit def mdAttribute2HBaseRichAttribute[T](mdAttribute:MdAttribute[T])(implicit context:HBaseDatabaseContext) =
                                                                        new HBaseRichAttribute[T](mdAttribute)(context)
}

import HBaseRichObjects._

class HBaseRichType(val mdType:MdType)(val context:HBaseDatabaseContext){
  val helper = context.hbaseHelper

  def tableName = mdType.name
  def fieldFamily = "ff_" + mdType.name
  def table = helper.getTable(tableName, Some(fieldFamily))

  def makeGet(id:String) = helper.makeGet(id)
}

class HBaseRichAttribute[T](val attribute:MdAttribute[T])(val context:HBaseDatabaseContext){
  implicit val context_ = context
  val helper = context.hbaseHelper

  def fieldName = attribute.name
  def getValue(result:Result, mdType:MdType) = helper.getValue(attribute.attrType, result, mdType.fieldFamily, fieldName)
}

class HBaseRichDbObject(val dbObject:DbObject)(val context:HBaseDatabaseContext){
  implicit val context_ = context
  val helper = context.hbaseHelper

  def assignId = dbObject.setId(makeId)

  def objectToPutForCreate = {
    val mdType = dbObject.mdType

    val put = dbObject.values.foldLeft(helper.makePut(dbObject.id)){(put, entry) =>
      val attr = mdType.getAttributeByName(entry._1)
      helper.addToPut(put, mdType.fieldFamily, attr.name, attr.attrType.asInstanceOf[MdAttrDataType[Any]], entry._2)
    }

    addOptimisticLockingIfNeeded(put, dbObject)
  }

  def objectToPutForUpdate = {
    val mdType = dbObject.mdType

    mdType.optimisticLockingAttribute.map{attr => if (dbObject.optimisticLocking.isEmpty)
      MddExceptions.missingOptimisticLockingAttribute(dbObject)
    }

    dbObject.values.foldLeft(helper.makePut(dbObject.id)){(put, entry) => val (attrName, value) = entry
      mdType.getAttributeByName(attrName) match {
        case MdAttribute(_, MdType.OptimisticLockingColumnName, LongType, _, _, _) =>
          helper.addToPut(put, mdType.fieldFamily, MdType.OptimisticLockingColumnName, LongType, value.asInstanceOf[Long] + 1)
        case MdAttribute(_, name, attrType, _, _, _) =>
          helper.addToPut(put, mdType.fieldFamily, name, attrType, attrType.asInstanceOf(value))
        case attr =>
          throw new UnexpectedStateException("Did not find attribute for specified value: %s".format(attr.name))
      }
    }
  }

  private def makeId = {
    val mdType = dbObject.mdType
    mdType.idGenerationPolicy.policy match {
      case RandomIdPolicy => context.idFactory.makeRandomId
      case SeqIdPolicy => context.idFactory.makeSeqId(mdType.name, mdType.idGenerationPolicy.idTemplate)
      case SuppliedIdPolicy => throw new FeatureIsNotImplementedException("SuppliedIdPolicy")
      case _ => throw new UnexpectedStateException("Unknown ID factory policy: %s".format(mdType.idGenerationPolicy.policy.toString))
    }
  }

  private def addOptimisticLockingIfNeeded(put:Put, dbObject:DbObject) = {
    val mdType = dbObject.mdType
    mdType.optimisticLockingAttribute.map((attr:MdAttribute[_]) => {
      helper.addToPut(put, mdType.fieldFamily, attr.name, LongType, 1L)
      put
    }).getOrElse(put)
  }
}
