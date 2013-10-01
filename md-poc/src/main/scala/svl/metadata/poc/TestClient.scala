package svl.metadata.poc

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseContext, HBaseDatabase}
import svl.metadata.poc.md.database._
import MdQueryOperators._
import MdQueryBooleanOperators._
import MdSortingPolicy._
import svl.metadata.poc.md.database.solr.DefaultSolrEnv
import svl.scala.lib.PerformanceUtil
import svl.metadata.poc.md.helpers.MdObjectHelper
import svl.metadata.poc.md.dictionary.MdTypes
import svl.metadata.poc.md.database.DbObject

object TestClient extends App{
  val db = new HBaseDatabase
                              with DefaultHBaseDatabaseContext
                              with DefaultSolrEnv

  val s = db.connect

  val claimTypeName = "Claim"
  val claimType = MdTypes.getType(claimTypeName).getOrElse(null)

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
//
//    s.create(createdObjects.toList, claimType)
  })

//  createdObjects.foreach(MdObjectHelper.dump(_))

  def makeDbObj(index:Int) = {
    DbObjectBuilder(claimType).
      addAttribute("Insurance" -> "Farmers")(classOf[String]).
      addAttribute("Amount" -> 100.0 * (index + 1))(classOf[Double]).
      addAttribute("Item" -> "Car")(classOf[String]).build
  }

  def dump(dbObject:Option[DbObject]) = dbObject.map{MdObjectHelper.dump}.orElse{println("There is no object");null}

  val query = MdQueryBuilder("", claimType)
    .filter(MdQueryConstrain(claimType.getAttributeByNameType("Amount")(classOf[Double]), Greater, 9100.0))
    .filter(MdQueryConstrain(And, claimType.getAttributeByNameType("Item")(classOf[String]), Equal, "Car"))
    .sortBy(claimType.getAttributeByName("Amount"), Descending)
    .startWith(0)
    .maxCount(5)
    .build

  val (result, queryTiming) = PerformanceUtil.timerWithResult(s.query(query))

  result.foreach(MdObjectHelper.dump)

  println ("Create                Timing: " + createTiming)
  println ("Create with Batching  Timing: " + createWithBatchingTiming)
  println ("Query                 Timing: " + queryTiming + "  Size: " + result.size)


  val id = "ClaimID-00000174"

  val obj1 = s.fetch(id, claimType)
  dump(obj1)
  val obj2 = changeObject(s.fetch(obj1.get))
  dump(obj2)
  val obj3 = changeObject(s.fetch(obj2.get))
  dump(obj3)
  val obj4 = changeObject(s.fetch(obj3.get))
  dump(obj4)


  def changeObject(dbObj:Option[DbObject]) = {
    dbObj.map {obj =>
        val amount = obj.mdType.getAttributeByNameType("Amount")(classOf[Double])
        val updated = DbObjectBuilder(obj).addAttribute(amount, obj.getValue(amount).getOrElse(0.0) + 1000.0).build
        s.update(updated)
        updated
    }
  }

  s.disconnect()
}


