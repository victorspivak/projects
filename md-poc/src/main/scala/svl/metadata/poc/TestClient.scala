package svl.metadata.poc

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseEnv, HBaseDatabase}
import svl.metadata.poc.md.mdd.{MdIdGenerationPolicies, DoubleAttributeBuilder, MdTypeBuilder, StringAttributeBuilder}
import svl.metadata.poc.md.database.DbObjectBuilder

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

  val createdObj = s.create(dbObj)
  println("Id= " + createdObj.id)

  s.disconnect()
}

