package svl.metadata.poc

import svl.metadata.poc.md.database.hbase.{DefaultHBaseDatabaseEnv, HBaseDatabase}

object TestClient extends App{
  val db = new HBaseDatabase with DefaultHBaseDatabaseEnv

  val s = db.connect
  for (i <- 1 to 5) {
    //    println(db.env.idFactory.makeRandomId)
    println(db.env.idFactory.makeSeqId("TestIdGr1", "gr1_%d"))
  }

  s.delete("12345")

  s.disconnect()
}

