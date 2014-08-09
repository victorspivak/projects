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
      case _ => throw new RuntimeException(s"Unknown $attrName attribute in the $name type")
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

  trait DbObjectBuilder[T<:ObjectType] {
    def objectType:T
    def overwriteValues:Boolean

    var values = scala.collection.mutable.HashMap[Attribute[Any, T], Any]()
    def newValues = values.toMap

    def add[D1, D2](entry:(Attribute[D1, T], D2))(implicit same:D1=:=D2):DbObjectBuilder[T] = add(entry._1, entry._2)
    def add[D1, D2](attribute:Attribute[D1, T], value:D2)(implicit same:D1=:=D2) = {
      if (!objectType.containsAttribute(attribute.name))
        throw new RuntimeException(s"Unknown ${attribute.name} attribute in the ${objectType.name} type")
      if (!overwriteValues && values.contains(attribute))
        throw new RuntimeException(s"Duplicate ${attribute.name} attribute")
      values += attribute -> value
      this
    }

    def build:DbObject[T]
  }

  class GenericDbObjectBuilder[T<:ObjectType](val objectType:T, val overwriteValues:Boolean = false) extends DbObjectBuilder[T]{
    def build:DbObject[T] = new GenericDbObjectBuilder.GenericDbObject(objectType, newValues)
  }

  object GenericDbObjectBuilder {
    case class GenericDbObject[T<:ObjectType] private[GenericDbObjectBuilder] (objectType:T, values:Map[Attribute[Any, T], Any]) extends DbObject[T] {
      override def setId(id:ObjId) = GenericDbObject(objectType, values + (objectType.id.asInstanceOf[Attribute[ObjId, T]] -> id) )
    }

    def apply(objectType:ObjectType) = new GenericDbObjectBuilder[objectType.type](objectType)

    def apply[T<:ObjectType](dbObject:DbObject[T]) = {
      val builder:DbObjectBuilder[T] = new GenericDbObjectBuilder(dbObject.objectType, true)
      dbObject.values.foldLeft(builder)(_.add(_))
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

  class UserRecordBuilder(val overwriteValues:Boolean = false) extends DbObjectBuilder[UserType.type]{
    def objectType = UserType

    def name(value:String) = add(UserType.userName -> value)
    def email(value:String) = add(UserType.userEmail -> value)
    def age(value:Int) = add(UserType.userAge -> value)

    def build:UserRecordBuilder.UserRecord = UserRecordBuilder.UserRecord(newValues)

    override def add[D1, D2](entry:(Attribute[D1, UserType.type], D2))(implicit same:D1=:=D2):UserRecordBuilder = super.add(entry).asInstanceOf[UserRecordBuilder]
  }

  object UserRecordBuilder {
    case class UserRecord private[UserRecordBuilder](values:Map[Attribute[Any, UserType.type], Any]) extends DbObject[UserType.type]{
      val objectType = UserType
      def setId(id:ObjId):UserRecord = UserRecord(values + (UserType.userId -> id))

      def name = getValue(UserType.userName)
      def email = getValue(UserType.userEmail)
      def age = getValue(UserType.userAge)
    }

    def apply() = new UserRecordBuilder()

    def apply(userRecord:UserRecord) = {
      val builder:UserRecordBuilder = new UserRecordBuilder(true)
      userRecord.values.foldLeft(builder)((builder, entry) => builder.add(entry))
      builder
    }
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

  val user1 = UserRecordBuilder().name("Vic").age(33).build

  dumpObject(user1)
  val user11 = user1.setId(ObjId("User1"))
  dumpObject(user11)

  val user2 = GenericDbObjectBuilder(UserType).
    add(userName -> "Victor").
    add(userEmail -> "vic@box.com").
    add(userAge -> 11).
//    add(fileName -> "My Favorite File").
    build
  dumpObject(user2)

  val file1 = GenericDbObjectBuilder(FileType).add(fileName -> "My Favorite Type").
//    add(userEmail -> "vic@box.com").
    build
  dumpObject(file1)

  println("==================================================")
  val userName1 = user1.getValue(userName)
  val userAge1 = user1.getValue(userAge)
  println(s"$userName1  --> $userAge1")

  //println(user1.getValue(fileName))

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
  val userUpdated1 = UserRecordBuilder(user1).name("John").
    age(22).
    add(userAge -> 11).
    build
  dumpObject(userUpdated1)
  println(userUpdated1.email)
}
