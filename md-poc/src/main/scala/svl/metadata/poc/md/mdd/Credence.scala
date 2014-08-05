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

  case class Attribute[+D, +T<:ObjectType](objectType:T, name:String, attrType:AttrDataType[D], size:Int) {
    def attrValueToString(value:Any) = value.asInstanceOf[D].toString
  }

  trait ObjectType {
    def typeId:String
    def name:String
    def attributes:List[Attribute[Any, ObjectType]]
    def id:Attribute[ObjId, ObjectType]
    def optimisticLocking:Option[Attribute[OptimisticLocking, ObjectType]]

    lazy val attributesByName:Map[String, Attribute[Any, ObjectType]] =
      attributes.foldLeft(Map[String, Attribute[Any, ObjectType]]())((m:Map[String, Attribute[Any, ObjectType]], a:Attribute[Any, ObjectType]) => m + (a.name -> a))

    def getAttributeByNameOpt(attrName:String) = attributesByName.get(attrName)

    def getAttributeByName(attrName:String):Attribute[Any, ObjectType] = getAttributeByNameOpt(attrName) match {
      case Some(attr) => attr
      case _ => throw MddExceptions.unknownAttribute(name, attrName)
    }

    def getAttributeByNameType[T](attrName:String)(clazz:Class[T]):Attribute[T, ObjectType] = getAttributeByName(attrName).asInstanceOf[Attribute[T, ObjectType]]

    def getAttributeByNameManifest[T](attrName:String)(implicit m:Manifest[T]):Attribute[T, this.type] =
      getAttributeByName(attrName).asInstanceOf[Attribute[T, this.type]]

    def containsAttribute(attrName:String) = attributesByName.contains(attrName)
  }

  case class GenericObjectType(typeId:String, name:String, attributes:List[Attribute[Any, GenericObjectType]],
                               id:Attribute[ObjId, GenericObjectType],
                               optimisticLocking:Option[Attribute[OptimisticLocking, GenericObjectType]] = None) extends ObjectType

  trait DbObject[T<:ObjectType]{
    def objectType:T
    def id:Option[ObjId] = getValue(objectType.id.asInstanceOf[Attribute[ObjId, T]])
    def values:Map[Attribute[Any, T], Any]

    def getValue[D](attr:Attribute[D, T]):Option[D] = values.get(attr).map(_.asInstanceOf[D])
    def getValue[D](attr:Option[Attribute[D, T]]):Option[D] = attr.flatMap((a:Attribute[D, T]) => getValue(a))

    def getAllValues:List[(Attribute[Any, T], Any)] = values.foldLeft(List[(Attribute[Any, T], Any)]()){(list, entry) =>
      entry._1 -> entry._2 ::list}
    def setId(id:ObjId):DbObject[T]
  }

  case class GenericDbObject[T<:ObjectType](objectType:T, values:Map[Attribute[Any, T], Any]) extends DbObject[T] {
    override def setId(id:ObjId) = GenericDbObject(objectType, values + (objectType.id.asInstanceOf[Attribute[ObjId, T]] -> id) )
  }

  class DbObjectBuilder[T<:ObjectType](objectType:T, overwriteValues:Boolean = false) {
    var values:scala.collection.mutable.HashMap[Attribute[_, T], Any] = scala.collection.mutable.HashMap[Attribute[Any, T], Any]()

    def add(entry:(String, Any)):DbObjectBuilder[T] = {
      val (name, value) = entry
      if (!objectType.containsAttribute(name))
        throw new RuntimeException(s"Unknown $name attribute in the ${objectType.name} type")
      val attribute = objectType.getAttributeByName(name).asInstanceOf[Attribute[Any,T]]
      if (!overwriteValues && values.contains(attribute))
        throw new RuntimeException(s"Duplicate $name attribute")
      values += attribute -> value
      this
    }

    private def addAttributeImpl[D](entry:(Attribute[D, T], D)) = add(entry._1.name -> entry._2)
    def addAttribute[D](entry:(Attribute[D, T], D)) = add(entry._1.name -> entry._2)
    def addAttribute[D](entry:(String, D))(implicit m:Manifest[D]) = {
      val (name, value) = entry
      addAttributeImpl(objectType.getAttributeByNameManifest(name) -> value)
    }

    def build = new GenericDbObject(objectType, values.toMap.asInstanceOf[Map[Attribute[Any, T], Any]])
  }

  object DbObjectBuilder {
    def apply(objectType:ObjectType) = new DbObjectBuilder[objectType.type](objectType)

    def apply[T<:ObjectType](dbObject:DbObject[T]) = dbObject.values.foldLeft(new DbObjectBuilder(dbObject.objectType, true)) {
      (builder, entry) => builder.addAttributeImpl(entry)
    }
  }

//----------------------------------------------------------------------------------------------------------------------
  object UserType extends ObjectType{
    def typeId = "UserTypeId-1"
    def name = "User"
    val userId = Attribute(this, "UserId", ObjIdType, 32)
    val userName = Attribute(this, "UserName", StringType, 32)
    val userEmail = Attribute(this, "UserEmail", StringType, 64)
    val userAge = Attribute(this, "UserAge", IntegerType, 4)
    val attributes = List(userId, userName, userEmail, userAge)

    val id = userId
    val optimisticLocking = None
  }

  object FileType extends ObjectType{
    def typeId = "FileTypeId-1"
    def name = "File"
    val fileId = Attribute(this, "FileId", ObjIdType, 32)
    val fileName = Attribute(this, "FileName", StringType, 32)
    val fileSize = Attribute(this, "FileSize", IntegerType, 4)
    val fileOwner = Attribute(this, "FileOwner", ObjIdType, 32)
    val attributes = List(fileId, fileName, fileSize, fileOwner)

    val id = fileId
    val optimisticLocking = None
  }

  case class UserRecord(values:Map[Attribute[Any, UserType.type], Any]) extends DbObject[UserType.type]{
    val objectType = UserType
    def setId(id:ObjId):UserRecord = UserRecord(values + (UserType.userId -> id))

    def name = getValue(UserType.userName)
    def email = getValue(UserType.userEmail)
    def age = getValue(UserType.userAge)
  }

  def dumpAttr(attr:Attribute[_, _]) {
    println(s"${attr.name}  --> ${attr.attrType.dataTypeName}  --> ${attr.objectType}")
  }

  def dumpObject[T<:ObjectType](obj:DbObject[T]) {
    println("==================================================")
    println(s"Object ${obj.id}")
    obj.getAllValues.foreach { entry =>
      val (attr, value) = entry
      println(s"${attr.name}  --> $value")
    }
  }

  import UserType._
  import FileType._

  println(Try(UserType.getAttributeByName("WrongName")))
  dumpAttr(UserType.getAttributeByName("UserName"))
  dumpAttr(UserType.userAge)

  val user1 = UserRecord(Map(userName -> "Vic", userAge -> 33))

  dumpObject(user1)
  val user11 = user1.setId(ObjId("User1"))
  dumpObject(user11)

  println(UserType.userEmail.objectType)
  println(FileType.fileName.objectType)

  val user2 = DbObjectBuilder(UserType).addAttribute(userName -> "Victor").
    addAttribute(userEmail -> "vic@box.com").
//    addAttribute(fileName -> "My Favorite File").
    build
  dumpObject(user2)

  val file1 = DbObjectBuilder(FileType).addAttribute(fileName -> "My Favorite Type").
//    addAttribute(userEmail -> "vic@box.com").
    build
  dumpObject(file1)

  println("==================================================")
  val userName1 = user1.getValue(userName)
  val userAge1 = user1.getValue(userAge)

  //println(user1.getValue(fileName))

  println(s"$userName1  --> $userAge1")

  def foo(user:DbObject[UserType.type]) {
    for {
      name <- user.getValue(userName)
      age <- user.getValue(userAge)
    } yield println(s"$name -> $age")
  }

  println("==================================================")
  foo(user1)
  foo(user2)

  println("==================================================")
  dumpObject(user1)
  val user111 = DbObjectBuilder(user1).addAttribute(userName -> "John").build
  dumpObject(user111)
  println(user1.email)
}
