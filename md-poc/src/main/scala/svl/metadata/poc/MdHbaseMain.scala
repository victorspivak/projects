package svl.metadata.poc

import org.apache.hadoop.hbase.client._
import org.apache.hadoop.hbase.util.Bytes
import org.apache.hadoop.hbase.HBaseConfiguration

class IteratorWrapper[A](iter:java.util.Iterator[A])
{
  def foreach(f: A => Unit): Unit = {
    while(iter.hasNext){
      f(iter.next)
    }
  }
}

object MdHbaseMain extends App {
  implicit def iteratorToWrapper[T](iter:java.util.Iterator[T]):IteratorWrapper[T] = new IteratorWrapper[T](iter)


  val conf = HBaseConfiguration.create()
  val pool = new HTablePool()
  val invoices:HTableInterface = pool.getTable(Bytes.toBytes("invoice"))

  val scan = new Scan()

  val invs = invoices.getScanner(scan)
  val it = invs.iterator()

  for (inv <- invs.iterator()) {
    val id = Bytes.toString(inv.getRow)
    val vendor = Bytes.toString(inv.getValue(Bytes.toBytes("inv_cf"), Bytes.toBytes("vendor")))
    val amount = Bytes.toDouble(inv.getValue(Bytes.toBytes("inv_cf"), Bytes.toBytes("amount")))

    printf("ID %10s  Vendor %20s  --> %s\n", id, vendor, amount)
//
//    val amountAsDouble = 1. * Integer.parseInt(amount)
//    val put = new Put(inv.getRow)
//    put.add(Bytes.toBytes("inv_cf"), Bytes.toBytes("amount"), Bytes.toBytes(amountAsDouble))
//    invoices.put(put)
  }

  invoices.close()
}
