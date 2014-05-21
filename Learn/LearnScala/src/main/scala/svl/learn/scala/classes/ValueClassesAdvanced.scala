package svl.learn.scala.classes

import scala.language.implicitConversions

object ValueClassesAdvanced {

  case class TypeId(id:String) extends AnyVal
  case class EntityType(name:TypeId)

  trait Record

  trait Entity[T<:AnyVal, R<:Record]{
    def entityId:T
    def entityType:EntityType
    def record:Option[R]
  }

  case class UserId(id:String) extends AnyVal
  case class PictureId(id:String) extends AnyVal
  class PictureRecord extends Record
  class Picture[T <: PictureRecord](val entityId:PictureId, val record:Option[T]) extends Entity[PictureId, T]{
    override def entityType = EntityType(TypeId("Picture"))
  }

  class UserRecord(val name:String) extends Record

  class CustomUserRecord(name:String, val email:String) extends UserRecord(name)

  class User[T <: UserRecord](val id:UserId, val record:Option[T])

  object User{
    def apply(id:UserId) = new User(id, None)
    def apply[T <: UserRecord](id:UserId, record:T) = new User(id, Some(record))
  }

  def main(args: Array[String]) {
    val user = User(UserId("12345"))
    println(user.record)
    val user1 = User(UserId("12345"), new UserRecord("User Name"))
    user1.record.map(r => println(r.name))
    val user2 = User(UserId("12345"), new CustomUserRecord("Custom User Name", "email@box.com"))
    user2.record.map(r => println(r.name))
    user2.record.map(r => println(r.email))
  }
}

