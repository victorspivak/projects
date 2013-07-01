package svl.metadata.poc

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseContext, HBaseDatabase}
import svl.metadata.poc.md.mdd.{MdIdGenerationPolicies, DoubleAttributeBuilder, MdTypeBuilder, StringAttributeBuilder}
import svl.metadata.poc.md.database._
import MdQueryOperators._
import MdQueryBooleanOperators._
import MdSortingPolicy._
import svl.metadata.poc.md.database.solr.DefaultSolrEnv
import svl.scala.lib.PerformanceUtil

object TestClient extends App{
  val db = new HBaseDatabase
                              with DefaultHBaseDatabaseContext
                              with DefaultSolrEnv

  val s = db.connect

  val typeName = "Claim"
  val claimId = StringAttributeBuilder("ClaimId").build
  val insurance = StringAttributeBuilder("Insurance").build
  val amount = DoubleAttributeBuilder("Amount").doFiltering().build
  val item = StringAttributeBuilder("Item").doFiltering().build
  val mdType = MdTypeBuilder(typeName).id(claimId).add(insurance).add(amount).add(item).
                    doOptimisticLocking().use(MdIdGenerationPolicies.SeqIdPolicy, "ClaimID-%08d").build
//  val dbObj = DbObjectBuilder(mdType).addAttribute(insurance -> "Farmers").addAttribute(amount -> 123.12).addAttribute(item -> "Car").build

  val createTiming = PerformanceUtil.timer({
//    for (i <- 1 to 100) {
//      val dbObj = makeDbObj(i)
//      val createdObj = s.create(dbObj)
////      val fetchedObject = s.fetch(createdObj.id, mdType)
////      dump(fetchedObject)
//    }
  })

  val (createdObjects, createWithBatchingTiming) = PerformanceUtil.timerWithResult({
//    val createdObjects = for {i <- 1 to 100
//      dbObj = makeDbObj(i)
//    } yield dbObj
//
//
//    s.create(createdObjects.toList, mdType)
  })

//  createdObjects.foreach(dumpDbObj(_))

  def makeDbObj(index:Int) = {
    DbObjectBuilder(mdType).addAttribute(insurance -> "Farmers").addAttribute(amount -> 100.0 * (index + 1)).addAttribute(item -> "Car").build
  }

  def dump(dbObject:Option[DbObject]) = dbObject.map(dumpDbObj(_)).orElse{println("There is no object");null}

  def dumpDbObj(obj:DbObject) {
      printf("Obj: %s  %s  %f ---> %s    OptLocking %d\n",
        obj.id,
        obj.getValue(insurance).getOrElse(""),
        obj.getValue(amount).getOrElse(0.0),
        obj.getValue(item).getOrElse(""),
        obj.optimisticLocking.getOrElse(0L))
  }

  val query = MdQueryBuilder("", mdType)
    .filter(MdQueryConstrain(amount, Greater, 9100.0))
    .filter(MdQueryConstrain(And, item, Equal, "Car"))
    .sortBy(amount, Descending)
    .startWith(0)
    .maxCount(200)
    .build

  val (result, queryTiming) = PerformanceUtil.timerWithResult(s.query(query))

  result.foreach(dumpDbObj(_))

  println ("Create                Timing: " + createTiming)
  println ("Create with Batching  Timing: " + createWithBatchingTiming)
  println ("Query                 Timing: " + queryTiming + "  Size: " + result.size)


  val id = "ClaimID-00000274"

  val obj1 = s.fetch(id, mdType)
  dump(obj1)
  val obj2 = changeObject(s.fetch(obj1.get))
  dump(obj2)
  val obj3 = changeObject(s.fetch(obj2.get))
  dump(obj3)
  val obj4 = changeObject(s.fetch(obj3.get))
  dump(obj4)


  def changeObject(dbObj:Option[DbObject]) = {
    dbObj.map {obj =>
        val updated = DbObjectBuilder(obj).addAttribute(amount, obj.getValue(amount).getOrElse(0.0) + 1000.0).build
        s.update(updated)
        updated
    }
  }

  s.disconnect()
}


