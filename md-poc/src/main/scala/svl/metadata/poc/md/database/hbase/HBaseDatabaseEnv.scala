package svl.metadata.poc.md.database.hbase

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.hbase.client._
import svl.metadata.poc.md.database.DbSession
import org.apache.hadoop.hbase.{HColumnDescriptor, HTableDescriptor, HBaseConfiguration}
import java.util.{Date, UUID}
import svl.metadata.poc.md.mdd.MdAttrDataTypes._
import org.apache.hadoop.hbase.util.Bytes
import svl.metadata.poc.md.mdd.MddBaseException

trait HBaseDatabaseEnv{
  def conf:Configuration
  def pool:HTablePool
  def sessionFactory:HBaseSessionFactory
  def administration:Administration
  def idFactory:IdFactory

  def helper = new HBaseHelper(this)

  trait HBaseSessionFactory{
    def newSession:DbSession
  }

  trait Administration{
    def adminSession:HBaseAdmin
  }

  trait IdFactory{
    def makeRandomId:String
    def makeSeqId(prefix:String, template:String):String
  }
}

trait DefaultHBaseDatabaseEnv{
  val hbaseEnv:HBaseDatabaseEnv = new HBaseDatabaseEnv{
    val conf = HBaseConfiguration.create()
    val pool = new HTablePool()
    def sessionFactory = new HBaseSessionFactory {
      def newSession = new HBaseSession(hbaseEnv)
    }

    def administration = new Administration{
      def adminSession = new HBaseAdmin(conf)
    }

    def idFactory = new IdFactory{
      val FactoryIdTableName = "IdFactory"
      val FactoryIdFieldFamily = "id_ff"
      val FactoryIdFieldName = "lastId"

      def makeRandomId:String = UUID.randomUUID().toString

      def makeSeqId(idGroup:String, template:String):String = {
        def makeId(id:Long, template:String) = template.format(id)
        val idTable = helper.getTable(FactoryIdTableName, Option(FactoryIdFieldFamily))
        val result = idTable.increment(helper.makeIncrement(idGroup, FactoryIdFieldFamily, FactoryIdFieldName, 1))
        helper.getValue(LongType, result, FactoryIdFieldFamily, FactoryIdFieldName) match {
          case Some(id) => makeId(id, template)
          case None => throw new Exception("Unexpected case. Should analyze.")
        }

      }
    }
  }
}

class HBaseHelper(val hbaseEnv:HBaseDatabaseEnv){
  implicit def string2Bytes(value:String) = Bytes.toBytes(value)

  def getTable(name:String, fieldFamily:Option[String]):HTableInterface = {
    try{
      hbaseEnv.pool.getTable(name)
    } catch {
      case exception:Exception => translateException(exception) match {
        case e:org.apache.hadoop.hbase.TableNotFoundException =>  fieldFamily.map(fldFamily => {
          createTable(name, fldFamily)
          getTable(name, None)
        }).getOrElse(null)
        case e:Exception => throw e
      }
    }
  }

  def translateException(exception:Throwable):Throwable = exception match {
    case e:MddBaseException => e
    case e:org.apache.hadoop.hbase.TableNotFoundException => e
    case e:Exception =>   if (e.getCause != null) translateException(e.getCause) else e
  }

  def closeTable(table:HTableInterface) {table.close()}

  def toBytes[T](attrType:MdAttrDataType[T], value:T) = attrType match {
    case StringType => Bytes.toBytes(value.asInstanceOf[String])
    case IntegerType => Bytes.toBytes(value.asInstanceOf[Int])
    case DoubleType => Bytes.toBytes(value.asInstanceOf[Double])
    case LongType => Bytes.toBytes(value.asInstanceOf[Long])
    case DateType => Bytes.toBytes(value.asInstanceOf[Date].getTime)
    case _ => throw new IllegalArgumentException(
      "The argument type %s is not supported in the toBytes conversion.".format(attrType.toString))
  }

  def makePut[T](id:String):Put = new Put(id)

  def makePut[T](id:String, fieldFamily:String, fieldName:String, attrType:MdAttrDataType[T], value:T):Put =
    addToPut(makePut(id), fieldFamily, fieldName, attrType, value)

  def addToPut[T](put:Put, fieldFamily:String, fieldName:String, attrType:MdAttrDataType[T], value:T) =
    put.add(fieldFamily, fieldName, toBytes(attrType, value))

  def makeIncrement(id:String, fieldFamily:String, fieldName:String, value:Long) =
    new Increment(id).addColumn(fieldFamily, fieldName, value)

  def makeGet(id:String) = new Get(id)

  def createTable(name:String, fieldFamily:String){
    val tableDesc = new HTableDescriptor(name)
    tableDesc.addFamily(new HColumnDescriptor(fieldFamily))
    hbaseEnv.administration.adminSession.createTable(tableDesc)
  }

  def isTableExist(name:String) = hbaseEnv.administration.adminSession.tableExists(name)
  def createTableIfMissing(name:String, fieldFamily:String) {
    if (!isTableExist(name))
      createTable(name, fieldFamily)
  }

  def getValue[T](attrType:MdAttrDataType[T], getResult:Result, fieldFamily:String, fieldName:String):Option[T] = {
    val values = getResult.getColumn(fieldFamily, fieldName)
    if (values.isEmpty)
      None
    else {
      val value = fromBytes(attrType, values.get(0).getValue)
      Some(value)
    }
  }

  private def fromBytes[T](attrType:MdAttrDataType[T], bytes:Array[Byte]):T = {
    attrType match {
      case StringType =>  Bytes.toString(bytes).asInstanceOf[T]
      case IntegerType => Bytes.toString(bytes).asInstanceOf[T]
      case DoubleType => Bytes.toDouble(bytes).asInstanceOf[T]
      case DateType => new Date(Bytes.toLong(bytes)).asInstanceOf[T]
      case LongType => Bytes.toLong(bytes).asInstanceOf[T]
    }
  }
}

