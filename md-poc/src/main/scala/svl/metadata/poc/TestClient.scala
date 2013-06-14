package svl.metadata.poc

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseEnv, HBaseDatabase}
import svl.metadata.poc.md.mdd.{MdIdGenerationPolicies, DoubleAttributeBuilder, MdTypeBuilder, StringAttributeBuilder}
import svl.metadata.poc.md.database.{DbObject, DbObjectBuilder}

object TestClient extends App{
  val db = new HBaseDatabase with DefaultHBaseDatabaseEnv

  val s = db.connect

  val typeName = "Claim"
  val claimId = StringAttributeBuilder("ClaimId").build
  val insurance = StringAttributeBuilder("Insurance").build
  val amount = DoubleAttributeBuilder("Amount").build
  val item = StringAttributeBuilder("Item").build
  val mdType = MdTypeBuilder(typeName).id(claimId).add(insurance).add(amount).add(item).
                    doOptimisticLocking().use(MdIdGenerationPolicies.SeqIdPolicy, "ClaimID-%08d").build
  val dbObj = DbObjectBuilder(mdType).addAttribute(insurance -> "Farmers").addAttribute(amount -> 123.12).addAttribute(item -> "Car").build

  for (i <- 1 to 10) {
    val createdObj = s.create(dbObj)
    val fetchedObject = s.fetch(createdObj.id, mdType)
    dump(fetchedObject)
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
  s.disconnect()
}

