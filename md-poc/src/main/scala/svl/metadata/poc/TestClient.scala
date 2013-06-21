package svl.metadata.poc

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseContext, HBaseDatabase}
import svl.metadata.poc.md.mdd.{MdIdGenerationPolicies, DoubleAttributeBuilder, MdTypeBuilder, StringAttributeBuilder}
import svl.metadata.poc.md.database._
import MdQueryOperators._
import MdQueryBooleanOperators._
import svl.metadata.poc.md.database.solr.DefaultSolrEnv

object TestClient extends App{
  val db = new HBaseDatabase
                              with DefaultHBaseDatabaseContext
                              with DefaultSolrEnv

  val s = db.connect

  val typeName = "Claim"
  val claimId = StringAttributeBuilder("ClaimId").build
  val insurance = StringAttributeBuilder("Insurance").build
  val amount = DoubleAttributeBuilder("Amount").build
  val item = StringAttributeBuilder("Item").build
  val mdType = MdTypeBuilder(typeName).id(claimId).add(insurance).add(amount).add(item).
                    doOptimisticLocking().use(MdIdGenerationPolicies.SeqIdPolicy, "ClaimID-%08d").build
//  val dbObj = DbObjectBuilder(mdType).addAttribute(insurance -> "Farmers").addAttribute(amount -> 123.12).addAttribute(item -> "Car").build

  for (i <- 1 to 100) {
    val dbObj = makeDbObj(i)
    val createdObj = s.create(dbObj)
//    val fetchedObject = s.fetch(createdObj.id, mdType)
//    dump(fetchedObject)
  }

  def makeDbObj(index:Int) = {
    DbObjectBuilder(mdType).addAttribute(insurance -> "Farmers").addAttribute(amount -> 100. * (index + 1)).addAttribute(item -> "Car").build
  }

  def dump(dbObject:Option[DbObject]) {
    dbObject.map{obj =>
      printf("Obj: %s  %s  %f ---> %s    OptLocking %d\n",
        obj.id,
        obj.getValue(insurance).getOrElse(""),
        obj.getValue(amount).getOrElse(0.0),
        obj.getValue(item).getOrElse(""),
        obj.optimisticLocking.getOrElse(0L))
    }
  }

  val query = MdQueryBuilder("", mdType)
    .filter(MdQueryConstrain(amount, Greater, 300.))
    .filter(MdQueryConstrain(And, item, Equal, "Car"))
    .build

  val result = s.query(query)
  result.foreach(println(_))

  s.disconnect()
}

