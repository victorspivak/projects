package svl.learn.scala.classes

import scala.language.implicitConversions

object ValueClassesAdvanced {

  case class TypeId(id:String) extends AnyVal
  case class EntityType(id:TypeId, name:String)

  trait Record

  trait Entity[T<:AnyVal, R<:Record]{
    def entityId:T
    def entityType:EntityType
    def record:Option[R]
  }

  case class UserId(id:String) extends AnyVal
  case class PictureId(id:String) extends AnyVal
  class PictureRecord extends Record
  class Picture(val entityId:PictureId, val record:Option[PictureRecord]) extends Entity[PictureId, PictureRecord]{
    override def entityType = EntityType(TypeId("#picture"), "Picture")
  }

  class UserRecord(val name:String, val picture:Picture) extends Record
  class CustomUserRecord(name:String, val email:String, picture:Picture) extends UserRecord(name, picture)

  class User[T <: UserRecord](val entityId:UserId, val record:Option[T]) extends Entity[UserId, UserRecord]{
    override def entityType: EntityType = EntityType(TypeId("#userId"), "User")
  }

  object User{
    def apply(id:UserId) = new User(id, None)
    def apply[T <: UserRecord](id:UserId, record:T) = new User(id, Some(record))
  }

  implicit def TypeId2String(typeId:TypeId) = typeId.id
  implicit def PictureId2String(pictureId:PictureId) = pictureId.id
  implicit def UserId2String(userId:UserId) = userId.id
  
  def main(args: Array[String]) {
    val user = User(UserId("12345"))
    println(user.record)
    val user1 = User(UserId("12345"), new UserRecord("User Name",
      new Picture(PictureId("Pic#1"), Some(new PictureRecord()))))

    user1.record.map(r => println(r.name))
    val user2 = User(UserId("12345"), new CustomUserRecord("Custom User Name", "email@box.com",
      new Picture(PictureId("Pic#2"), Some(new PictureRecord()))))

    user2.record.map(r => println(r.name))
    user2.record.map(r => println(r.email))
    println(user2.entityType.name)
  }
}

