//package svl.mongo
//
//import com.mongodb.casbah.commons.MongoDBObject
//import com.mongodb.DBObject
//
//
//case class Invoice(invoiceNumber:String, orderId:String, vendor:String, amount:Int, description:String, approved:Boolean) extends AbstractEntity {
//  def this(dbObj:DBObject) = this(dbObj.get(Invoice.INVOICE_NUMBER).toString, dbObj.get(Invoice.ORDER_ID).toString,
//              dbObj.get(Invoice.VENDOR).toString, dbObj.get(Invoice.AMOUNT).toString.toInt,
//              dbObj.get(Invoice.DESCRIPTION).toString, dbObj.get(Invoice.APPROVED).toString.toBoolean)
//  val dbObj:DBObject = MongoDBObject(
//    Invoice.INVOICE_NUMBER -> invoiceNumber,
//    Invoice.ORDER_ID -> orderId,
//    Invoice.VENDOR -> vendor,
//    Invoice.AMOUNT -> amount,
//    Invoice.DESCRIPTION-> description,
//    Invoice.APPROVED -> approved)
//
//  def toDbObject = dbObj
//}
//
//object Invoice {
//  val INVOICE_NUMBER: String = "invoiceNumber"
//  val ORDER_ID: String = "orderId"
//  val VENDOR: String = "vendor"
//  val AMOUNT: String = "amount"
//  val DESCRIPTION: String = "description"
//  val APPROVED: String = "approved"
//
//  val ITS_APPROVED = true
//  val ITS_NOT_APPROVED = false
//}
//
//class InvoiceActor(collectionName:String)(implicit db:Db) extends EntityActor[Invoice](collectionName)(db) {
//  def convert(dbObj: DBObject): Invoice = new Invoice(dbObj)
//}
//
//object InvoiceActor {
//  def apply(collectionName:String)(implicit db:Db) = new InvoiceActor(collectionName)(db)
//}
