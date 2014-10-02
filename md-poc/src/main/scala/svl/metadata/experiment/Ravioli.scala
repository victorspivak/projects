package svl.metadata.experiment

import java.util.Date

import scala.collection.immutable.HashMap
import scala.reflect.Manifest

object Ravioli extends App{
  object AttrDataTypes{
    case class ObjId(imp:String) extends AnyVal
    case class OptimisticLocking(imp:Long) extends AnyVal
    case class SSN(imp:String) extends AnyVal

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
    val SsnType = AttrDataType[SSN]()
  }

  import svl.metadata.experiment.Ravioli.AttrDataTypes._

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

  trait AttributesBag[T<:ObjectType]{
    def objectType:T
    def values:Map[Attribute[Any, T], Any]

    def getValue[D](attr:Attribute[D, T]):Option[D] = values.get(attr).map(_.asInstanceOf[D])
    def getValue[D](attr:Option[Attribute[D, T]]):Option[D] = attr.flatMap((a:Attribute[D, T]) => getValue(a))

    def getAllValues:List[(Attribute[Any, T], Any)] = values.foldLeft(List[(Attribute[Any, T], Any)]()){(list, entry) =>
      entry._1 -> entry._2 ::list}
  }

  trait DbObject[T<:ObjectType] extends AttributesBag[T]{
    def objectType:T
    def id:Option[ObjId] = getValue(objectType.id.asInstanceOf[Attribute[ObjId, T]])
    def values:Map[Attribute[Any, T], Any]

    def setId(id:ObjId):DbObject[T]
  }

  trait DbObjectBuilder[T<:ObjectType]  extends AttributesBag[T] {
    def objectType:T
    def overwriteValues:Boolean

    var values = HashMap[Attribute[Any, T], Any]()
    def newValues = values.toMap

    def add[D1, D2](entry:(Attribute[D1, T], D2))(implicit same:D1=:=D2):this.type = add(entry._1, entry._2)
    def add[D1, D2](attribute:Attribute[D1, T], value:D2)(implicit same:D1=:=D2):this.type = {
      if (!objectType.containsAttribute(attribute.name))
        throw new RuntimeException(s"Unknown ${attribute.name} attribute in the ${objectType.name} type")
      if (!overwriteValues && values.contains(attribute))
        throw new RuntimeException(s"Duplicate ${attribute.name} attribute")
      values += attribute -> value
      this
    }

    def build:DbObject[T]
  }

  case class GenericDbObject[T<:ObjectType] (objectType:T, values:Map[Attribute[Any, T], Any]) extends DbObject[T] {
    override def setId(id:ObjId) = GenericDbObject(objectType, values + (objectType.id.asInstanceOf[Attribute[ObjId, T]] -> id) )
  }

  class GenericDbObjectBuilder[T<:ObjectType](val objectType:T, val overwriteValues:Boolean = false) extends DbObjectBuilder[T]{
    def build:DbObject[T] = new GenericDbObject(objectType, newValues)
  }

  object GenericDbObjectBuilder {
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
    val userPhone = Attribute(this, "UserPhone", StringType, 16)
    val userSsn = Attribute(this, "UserSSN", SsnType, 9)
    val attributes = List(userId, userName, userEmail, userSsn, userPhone)

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

  trait UserId {self:DbObject[UserType.type] =>
    def userId = getValue(UserType.userId)
  }

  trait UserName {self:DbObject[UserType.type] =>
    def name = getValue(UserType.userName)
  }

  trait UserEmail {self:DbObject[UserType.type] =>
    def email = getValue(UserType.userEmail)
  }

  trait UserPhone {self:DbObject[UserType.type] =>
    def phone = getValue(UserType.userPhone)
  }

  trait UserSsn {self:DbObject[UserType.type] =>
    def ssn = getValue(UserType.userSsn)
  }

  trait UserIdBuilder {self:DbObjectBuilder[UserType.type] =>
    def userId = getValue(UserType.userId)
    def userId_=(value:ObjId) = add(UserType.userId, value)
  }

  trait UserNameBuilder {self:DbObjectBuilder[UserType.type] =>
//    def name = getValue(UserType.userName)
    def name(value:String):this.type with UserName = add(UserType.userName -> value).asInstanceOf[this.type with UserName]
    def name_=(value:String) = add(UserType.userName -> value)
  }

  trait UserEmailBuilder {self:DbObjectBuilder[UserType.type] =>
//    def email = getValue(UserType.userEmail)
    def email(value:String):this.type with UserEmail = add(UserType.userEmail -> value).asInstanceOf[this.type with UserEmail]
    def email_=(value:String) = add(UserType.userEmail -> value)
  }

  trait UserPhoneBuilder {self:DbObjectBuilder[UserType.type] =>
    def phone = getValue(UserType.userPhone)
    def phone_=(value:String) = add(UserType.userPhone -> value)
  }

  trait UserSsnBuilder {self:DbObjectBuilder[UserType.type] =>
    def ssn = getValue(UserType.userSsn)
    def ssn_=(value:SSN) = add(UserType.userSsn -> value)
  }

  class UserRecordBuilder(val overwriteValues:Boolean = false) extends DbObjectBuilder[UserType.type]
    with UserIdBuilder with UserNameBuilder with UserEmailBuilder with UserPhoneBuilder with UserSsnBuilder {
    def objectType = UserType

    def build:UserRecordBuilder.UserRecord = UserRecordBuilder.UserRecord(newValues)
  }

  object UserRecordBuilder {
    case class UserRecord private[UserRecordBuilder](values:Map[Attribute[Any, UserType.type], Any])
      extends DbObject[UserType.type]
      with UserId with UserName with UserEmail with UserPhone with UserSsn {
      val objectType = UserType
      def setId(id:ObjId):UserRecord = UserRecord(values + (UserType.userId -> id))
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

  import svl.metadata.experiment.Ravioli.UserType._
  //import FileType._

  type UserWithNameAndSsn = DbObject[UserType.type] with UserName with UserSsn

  val user1 = new GenericDbObject(UserType, Map[Attribute[Any, UserType.type], Any](userName -> "Victor", userSsn -> "111-11-1111")) with UserName with UserSsn
  val user2 = new GenericDbObject(UserType, Map[Attribute[Any, UserType.type], Any](userName -> "Vic", userSsn -> "222-22-2222")) with UserName with UserSsn

  def dump(user:UserWithNameAndSsn) = println(s"${user.name} ---> ${user.ssn}")
  def dumpUserName(user:UserName) = println(s"${user.name}")

  dump(user1)
  dumpUserName(user2)

  val user3 = UserRecordBuilder().name("John").email("john@box.com").build
//  dump(user3)
  dumpObject(user3)

  val user4Builder = UserRecordBuilder().name("Yegor").email("1@2")
  user4Builder.ssn = SSN("xxxxxxxx")
  val user4 = user4Builder.build
  dumpObject(user4)
}
