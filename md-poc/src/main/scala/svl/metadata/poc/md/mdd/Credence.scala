package svl.metadata.poc.md.mdd

import java.util.Date

import scala.reflect.Manifest
import scala.util.Try

object Credence extends App {
  object AttrDataTypes{
    case class ObjId(imp:String) extends AnyVal
    case class OptimisticLocking(imp:Long) extends AnyVal

    sealed case class AttrDataType[+T:Manifest](){
      def dataTypeName = implicitly[Manifest[T]].toString()
      def asInstanceOf(value:Any) = value.asInstanceOf[T]
    }

    val StringType = AttrDataType[String]()
    val IntegerType = AttrDataType[Int]()
    val DoubleType = AttrDataType[Double]()
    val DateType = AttrDataType[Date]()
    val LongType = AttrDataType[Long]()

    val ObjIdType = AttrDataType[ObjId]()
    val OptimisticLockingType = AttrDataType[OptimisticLocking]()
  }

  import AttrDataTypes._

  case class Attribute[+T](id:String, name:String, attrType:AttrDataType[T], size:Int) {
    def attrValueToString(value:Any) = value.asInstanceOf[T].toString
  }

  trait ObjectType {
    def typeId:String
    def name:String
    def attributes:List[Attribute[Any]]
    def id:Attribute[ObjId]
    def optimisticLocking:Option[Attribute[OptimisticLocking]]

    lazy val attributesByName:Map[String, Attribute[Any]] =
      attributes.foldLeft(Map[String, Attribute[Any]]())((m:Map[String, Attribute[Any]], a:Attribute[Any]) => m + (a.name -> a))

    def getAttributeByNameOpt(attrName:String) = attributesByName.get(attrName)

    def getAttributeByName(attrName:String):Attribute[Any] = getAttributeByNameOpt(attrName) match {
      case Some(attr) => attr
      case _ => throw MddExceptions.unknownAttribute(name, attrName)
    }

    def getAttributeByNameType[T](attrName:String)(clazz:Class[T]):Attribute[T] = getAttributeByName(attrName).asInstanceOf[Attribute[T]]

    def getAttributeByNameManifest[T](attrName:String)(implicit m:Manifest[T]):Attribute[T] =
      getAttributeByName(attrName).asInstanceOf[Attribute[T]]

    def containsAttribute(attrName:String) = attributesByName.contains(attrName)
  }

  case class GenericObjectType(typeId:String, name:String, attributes:List[Attribute[Any]],
                               id:Attribute[ObjId],
                               optimisticLocking:Option[Attribute[OptimisticLocking]] = None) extends ObjectType

  trait DbObject{
    def id:Option[ObjId] = getValue(objectType.id)
    def objectType:ObjectType
    def values:Map[Attribute[Any], Any]

    def getValue[T](attr:Attribute[T]):Option[T] = values.get(attr).map(_.asInstanceOf[T])
    def getValue[T](attr:Option[Attribute[T]]):Option[T] = attr.flatMap((a:Attribute[T]) => getValue(a))

    def getAllValues:List[(Attribute[Any], Any)] = values.foldLeft(List[(Attribute[Any], Any)]()){(list, entry) =>
      entry._1 -> entry._2 ::list}
    def setId(id:ObjId):DbObject
  }

  case class GenericDbObject(objectType:ObjectType, values:Map[Attribute[Any], Any]) extends DbObject {
    override def setId(id:ObjId) = GenericDbObject(objectType, values + (objectType.id -> id) )
  }

  class DbObjectBuilder(objectType:ObjectType, overwriteValues:Boolean = false) {
    var values = scala.collection.mutable.HashMap[Attribute[Any], Any]()

    def add(entry:(String, Any)):DbObjectBuilder = {
      val (name, value) = entry
      val attribute = objectType.getAttributeByName(name)
      if (!objectType.containsAttribute(name))
        throw new RuntimeException(s"Unknown $name attribute in the ${objectType.name} type")
      if (!overwriteValues && values.contains(attribute))
        throw new RuntimeException(s"Duplicate $name attribute")
      values += attribute -> value
      this
    }

    private def addAttributeImpl[T](entry:(Attribute[T], T)):DbObjectBuilder = add(entry._1.name -> entry._2)
    def addAttribute[T](entry:(Attribute[T], T)):DbObjectBuilder = add(entry._1.name -> entry._2)
    def addAttribute[T](entry:(String, T))(implicit m:Manifest[T]):DbObjectBuilder = {
      val (name, value) = entry
      addAttributeImpl(objectType.getAttributeByNameManifest(name) -> value)
    }

    def build = new GenericDbObject(objectType, values.toMap)
  }

  object DbObjectBuilder {
    def apply(objectType:ObjectType) = new DbObjectBuilder(objectType)

    def apply(dbObject:DbObject) = dbObject.values.foldLeft(new DbObjectBuilder(dbObject.objectType, true)) {
      (builder, entry) => builder.addAttributeImpl(entry)
    }
  }

//----------------------------------------------------------------------------------------------------------------------

  object UserType extends ObjectType{
    def typeId = "UserTypeId-1"
    def name = "User"
    val userId = Attribute("user-name-id-1", "UserId", ObjIdType, 32)
    val userName = Attribute("user-name-id-1", "UserName", StringType, 32)
    val userEmail = Attribute("user-email-id-1", "UserEmail", StringType, 64)
    val userAge = Attribute("user-age-id-1", "UserAge", IntegerType, 4)
    val attributes = List(userId, userName, userEmail, userAge)

    val id = userId
    val optimisticLocking = None
  }

  case class UserRecord(values:Map[Attribute[Any], Any]) extends DbObject{
    val objectType = UserType
    def setId(id:ObjId):UserRecord = UserRecord(values + (UserType.userId -> id))

    def name = getValue(UserType.userName)
    def email = getValue(UserType.userEmail)
    def age = getValue(UserType.userAge)
  }

  def dumpAttr(attr:Attribute[_]) {
    println(s"${attr.name}  --> ${attr.attrType.dataTypeName}")
  }

  def dumpObject(obj:DbObject) {
    println("==================================================")
    println(s"Object ${obj.id}")
    obj.getAllValues.foreach { entry =>
      val (attr, value) = entry
      println(s"${attr.name}  --> $value")
    }
  }

  println(Try(UserType.getAttributeByName("WrongName")))
  dumpAttr(UserType.getAttributeByName("UserName"))
  dumpAttr(UserType.userAge)
  val user1 = UserRecord(Map(UserType.userName -> "Vic", UserType.userAge -> 33))
  dumpObject(user1)
  val user11 = user1.setId(ObjId("User1"))
  dumpObject(user11)

  val user2 = DbObjectBuilder(UserType).addAttribute(UserType.userName, "Victor").
    addAttribute(UserType.userEmail, "vic@box.com").
    build

  dumpObject(user2)
}
