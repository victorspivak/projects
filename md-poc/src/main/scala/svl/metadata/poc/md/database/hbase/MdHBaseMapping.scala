package svl.metadata.poc.md.database.hbase

import svl.metadata.poc.md.mdd.{FeatureIsNotImplementedException, MdAttribute, MdType}
import svl.metadata.poc.md.database.DbObject
import svl.metadata.poc.md.mdd.MdIdGenerationPolicies._
class MdHBaseMapping (val env:HBaseDatabaseEnv){
  def typeToTableName(mdType:MdType) = mdType.name
  def attributeToFieldFamily(mdType:MdType, mdAttribute:MdAttribute) = "ff_" + mdType.name

//  def createObjectToPut(dbObject:DbObject) = {
//    val mdType = dbObject.mdType
//    val put = env.helper.makePut(makeId(dbObject))
//
//    dbObject.
//  }

  def makeId(dbObject:DbObject) = {
    val mdType = dbObject.mdType
    mdType.idGenerationPolicy.policy match {
      case RandomIdPolicy => env.idFactory.makeRandomId
      case SeqIdPolicy => env.idFactory.makeSeqId(mdType.name, mdType.idGenerationPolicy.idTemplate)
      case SuppliedIdPolicy => throw new FeatureIsNotImplementedException("SuppliedIdPolicy")
    }
  }
}

