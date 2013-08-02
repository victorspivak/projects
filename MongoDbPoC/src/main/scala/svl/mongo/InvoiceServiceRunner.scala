package svl.mongo

import MongoInvoiceService._
import svl.scala.lib.PerformanceUtil

object InvoiceServiceRunner extends App {
  val filter1 = new AbstractFilter().filterEquals(Invoice.VENDOR, "ABC")
  val filter2 = new AbstractFilter().filterGT(Invoice.AMOUNT, 20000)

  println("Creation time: " + PerformanceUtil.timer({createInvoices(100000, "EMC", 300000, 10)}))
  println("Counting time: " + PerformanceUtil.timer({count()}))
  println("Query time: " + PerformanceUtil.timer({query(filter1)}))
}

