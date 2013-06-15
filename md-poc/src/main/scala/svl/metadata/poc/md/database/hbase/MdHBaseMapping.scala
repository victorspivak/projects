package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.{UnexpectedStateException, FeatureIsNotImplementedException, MdAttribute, MdType}
import svl.metadata.poc.md.database.DbObject
import svl.metadata.poc.md.mdd.MdIdGenerationPolicies._
import svl.metadata.poc.md.mdd.MdAttrDataTypes._
import org.apache.hadoop.hbase.client.Put

class MdRichType(val mdType:MdType){

}

object MdRichType{
  implicit def mdType2MdRichType(mdType:MdType) = new MdRichType(mdType)
}

class MdHBaseMapping (val env:HBaseDatabaseEnv){
  val helper = env.helper
  def typeToTableName(mdType:MdType) = mdType.name
  def attributeToFieldFamily(mdType:MdType) = "ff_" + mdType.name

  def objectToPutForCreate(dbObject:DbObject, id:String) = {
    val mdType = dbObject.mdType

    val put = dbObject.values.foldLeft(helper.makePut(id)){(put, entry) =>
      mdType.getAttributeByName(entry._1) match {
        case Some(attr:MdAttribute[_]) => helper.addToPut(put, attributeToFieldFamily(mdType), attr.name, attr.attrType, entry._2)
        case None => throw new UnexpectedStateException("Did not find attribute for specified value")
      }
    }

    addOptimisticLockingIfNeeded(put, dbObject)
  }

  def addOptimisticLockingIfNeeded(put:Put, dbObject:DbObject) = {
    val mdType = dbObject.mdType
    mdType.optimisticLockingAttribute.map((attr:MdAttribute[_]) => {
      helper.addToPut(put, attributeToFieldFamily(mdType), attr.name, LongType, 1L)
      put
    }).getOrElse(put)
  }

  def makeId(dbObject:DbObject) = {
    val mdType = dbObject.mdType
    mdType.idGenerationPolicy.policy match {
      case RandomIdPolicy => env.idFactory.makeRandomId
      case SeqIdPolicy => env.idFactory.makeSeqId(mdType.name, mdType.idGenerationPolicy.idTemplate)
      case SuppliedIdPolicy => throw new FeatureIsNotImplementedException("SuppliedIdPolicy")
    }
  }
}

object MdHBaseMapping {
  def apply(env:HBaseDatabaseEnv) = new MdHBaseMapping(env)
}
