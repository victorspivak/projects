package svl.scalaimp.prototype.Rest

import dispatch._
import scala.reflect.BeanInfo
import sjson.json.Serializer.SJSON

object RestClientUsingDispatch {
    def main(args: Array[String]) {

        @BeanInfo class User(val screen_name: String, val description: String) { def this() = this("", "") }
//        @BeanInfo class Users(val users:Array[User]) { def this() = this(new Array[User](0)) }
        @BeanInfo case class Users(users:List[User]) {def this() = this(List())}

//        val http = new Http
//        val json = http(:/("api.twitter.com") / "1/users/show.json" <<? Map("screen_name" -> "aloiscochard") >~ { _.getLines.mkString })
//
//        val user = SJSON.in[User](json)
//
//        println(user.description)
//
//        val me = new User("Victor", "Me")
//        println(SJSON.toJSON(me))
//        println(SJSON.in[User](SJSON.toJSON(me)).description)


//        val users = new Users(List(new User("vic spivak", "It is me"), new User("vad spivak", "it is my son")).toArray)
        val users = new Users(List(new User("vic spivak", "It is me"), new User("vad spivak", "it is my son")))

        val out = SJSON.toJSON(users)

        println(out)
        val users1 = SJSON.in[Users](out)

        println(users1)
    }
}
