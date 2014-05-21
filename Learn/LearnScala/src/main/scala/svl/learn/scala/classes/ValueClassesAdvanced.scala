package svl.learn.scala.classes

import scala.language.implicitConversions

object ValueClassesAdvanced {

  case class TypeId(id:String) extends AnyVal
  case class EntityType(name:TypeId)

  trait Entity[T<:AnyVal, R]{
    def entityId:T
    def entityType:EntityType
    def record:R
  }

  case class UserId(id:String) extends AnyVal
  case class PictureId(id:String) extends AnyVal
  class PictureRecord
  class Picture[T <: PictureRecord](val id:PictureId, record:Option[T])

  class UserRecord{
    def name = "User Name"
  }

  trait CustomUserRecord extends UserRecord{
    override def name: String = "Custom user name"
    def email = "email@box.com"
  }

  class User[T <: UserRecord](val id:UserId, val record:Option[T])

  object User{
    def apply(id:UserId) = new User(id, None)
    def apply[T <: UserRecord](id:UserId, record:T) = new User(id, Some(record))
  }

  def main(args: Array[String]) {
    val user = User(UserId("12345"))
    println(user.record)
    val user1 = User(UserId("12345"), new UserRecord{})
    user1.record.map(r => println(r.name))
    val user2 = User(UserId("12345"), new CustomUserRecord{})
    user2.record.map(r => println(r.name))
    user2.record.map(r => println(r.email))
  }
}

