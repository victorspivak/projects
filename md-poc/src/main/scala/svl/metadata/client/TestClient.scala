package svl.metadata.client

import svl.metadata.poc.md.database.MdQueryBooleanOperators._
import svl.metadata.poc.md.database.MdQueryOperators._
import svl.metadata.poc.md.database.MdSortingPolicy._
import svl.metadata.poc.md.database.{DbObject, _}
import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseContext, HBaseDatabase}
import svl.metadata.poc.md.database.solr.DefaultSolrEnv
import svl.metadata.poc.md.dictionary.MdTypes
import svl.metadata.poc.md.helpers.MdObjectHelper
import svl.scala.lib.PerformanceUtil

object TestClient extends App{
  val db = new HBaseDatabase
                              with DefaultHBaseDatabaseContext
                              with DefaultSolrEnv

  val session = db.connect

  val claimTypeName = "Claim"
  val claimType = MdTypes.getType(claimTypeName).orNull

  val amountAttr = claimType.getAttributeByNameType("Amount")(classOf[Double])
  val itemAttr = claimType.getAttributeByNameType("Item")(classOf[String])
  val insuranceAttr = claimType.getAttributeByNameType("Insurance")(classOf[String])

  val createTiming = PerformanceUtil.timer({
//    for (i <- 1 to 100) {
//      val dbObj = makeDbObj(i)
//      val createdObj = s.create(dbObj)
////      val fetchedObject = s.fetch(createdObj.id, claimType)
////      dump(fetchedObject)
//    }
  })

  val (createdObjects, createWithBatchingTiming) = PerformanceUtil.timerWithResult({
//    val createdObjects = for {i <- 1 to 100
//      dbObj = makeDbObj(i)
//    } yield dbObj
//
//    s.create(createdObjects.toList, claimType)
  })

//  createdObjects.foreach(MdObjectHelper.dump(_))

  def makeDbObj(index:Int) = {
    DbObjectBuilder(claimType).
      addAttribute(insuranceAttr -> "Farmers").
      addAttribute(amountAttr -> 100.0 * (index + 1)).
      addAttribute(itemAttr -> "Car").build
  }

  def dump(dbObject:Option[DbObject]) = dbObject.map{MdObjectHelper.dump}.orElse{println("There is no object");null}

  val query = MdQueryBuilder("", claimType)
    .filter(MdQueryConstrain(amountAttr, Greater, 9100.0))
    .filter(MdQueryConstrain(And, itemAttr, Equal, "Car"))
    .sortBy(amountAttr, Descending)
    .startWith(0)
    .maxCount(5)
    .build

  val (result, queryTiming) = PerformanceUtil.timerWithResult(session.query(query))

  result.foreach(MdObjectHelper.dump)

  println ("Create                Timing: " + createTiming)
  println ("Create with Batching  Timing: " + createWithBatchingTiming)
  println ("Query                 Timing: " + queryTiming + "  Size: " + result.size)


  val id = "ClaimID-00000001"

  val obj1 = session.fetch(id, claimType)
  dump(obj1)
  val obj2 = changeObject(session.fetch(obj1.get))
  dump(obj2)
  val obj3 = changeObject(session.fetch(obj2.get))
  dump(obj3)
  val obj4 = changeObject(session.fetch(obj3.get))
  dump(obj4)


  def changeObject(dbObj:Option[DbObject]) = {
    dbObj.map {obj =>
        val amount = obj.mdType.getAttributeByNameType("Amount")(classOf[Double])

      val value = obj.getValue(amount).getOrElse(0.0)
      val updated = DbObjectBuilder(obj).addAttribute(amountAttr, value + 1000.0).build
        session.update(updated)
        updated
    }
  }

  session.disconnect()
}


