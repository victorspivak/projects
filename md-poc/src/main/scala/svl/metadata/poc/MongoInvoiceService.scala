//package svl.mongo
//
//import com.mongodb.casbah.Imports
//import scala.util.Random
//import svl.scala.lib.generators.EntityTemplate.RandomString
//import svl.scala.lib.{StopWatch, ConsoleUtil}
//import java.io.OutputStream
//import scala.collection.mutable
//
//object MongoInvoiceService {
//  implicit val db = Db("localhost", "test")
//  val invoices = InvoiceActor("invoices")
//  val vendors = Array("Box", "EMC", "Documentum", "FileNet", "IBM", "Google", "Apple", "Sprint", "Dell", "HTC", "Samsung")
//  val maxVendorIndex = vendors.length - 1
//  val random = new Random
//
//  def createInvoices(count:Int, vendor:String, baseAmount:Int = 1000, amountDelta:Int = 1) {
//    for (i <- 0 to count) {
//      createInvoice("Inv#000" + i, "Ord00000" + i, vendor, baseAmount + i * amountDelta, "it is an invoice", Invoice.ITS_APPROVED)
//    }
//  }
//
//  def createRandomInvoices(count:Int, batchSize:Int=1000)(out:OutputStream) {
//    val batchCount = count / batchSize
//
//    for (batchIndex <- 1 to batchCount) {
//      val stopWatch = StopWatch()
//      val batch = Array.iterate(makeRandomInvoice, batchSize) (i => makeRandomInvoice)
//      invoices.insert(batch: _*)
//      ConsoleUtil.printTiming(batchIndex * batchSize, count, batchSize, stopWatch)(out)
//    }
//
//    if (batchCount * batchSize < count) {
//      val stopWatch = StopWatch()
//      for (i <- batchCount * batchSize to count)
//        createInvoice(makeRandomInvoice)
//      ConsoleUtil.printTiming(count, count, batchSize, stopWatch)(out)
//    }
//  }
//
//  def makeRandomInvoice = {
//    val desc = RandomString("%s", 40)
//    val vendor = vendors(random.nextInt(maxVendorIndex))
//    Invoice("Inv#" + random.nextInt(1000000), "Ord" +  + random.nextInt(1000000), vendor, random.nextInt(100000), desc, random.nextBoolean())
//  }
//
//  def createInvoices(count:Int, inv: Invoice) {
//    for (i <- 1 to count) {
//      createInvoice(inv.invoiceNumber, inv.orderId, inv.vendor, inv.amount, inv.description, inv.approved)
//    }
//  }
//
//  def createInvoice(invoiceNumber: String, orderId: String, vendor: String, amount: Int, description: String, approveStatus: Boolean): Imports.WriteResult = {
//    val inv = Invoice(invoiceNumber, orderId, vendor, amount, description, approveStatus)
//    createInvoice(inv)
//  }
//
//  def createInvoice(inv: Invoice): Imports.WriteResult = {
//    invoices.insert(inv)
//  }
//
//  def count() = {
//    val count: Long = invoices.count(new AbstractFilter)
//    println("Count: " + count)
//    count
//  }
//
//  def query(filter:AbstractFilter, dump:Boolean = false) = {
//    val records = invoices.query(filter)
//    if (dump)
//      records.foreach(println(_))
//
//    records.size
//  }
//}
