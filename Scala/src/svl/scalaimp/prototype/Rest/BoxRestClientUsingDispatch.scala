package svl.scalaimp.prototype.Rest

import dispatch._
import scala.reflect.BeanInfo
import sjson.json.Serializer.SJSON

object BoxRestClientUsingDispatch {
    def main(args: Array[String]) {

		@BeanInfo case class User(name:String, id:String, login:String) { def this() = this("", "", "") }
		@BeanInfo case class Folder(name:String, id:String, size:Int, created_by:User) { def this() = this("", "", 0, new User) }

		//curl https://vspivak.inside-box.net/api/2.0/folders/11450952   -H "Authorization: BoxAuth api_key=3i8u7632f52ye9kh879gsnva7yb7ylw1&auth_token=x9d927uvd0vfv5ei5zo85d3wx8jw8e9n"
        val http = new Http
		val req  = :/("vspivak.inside-box.net") / "api/2.0/folders/11449764"
        val json = http(req.secure <:<  Map("Authorization" -> "BoxAuth api_key=3i8u7632f52ye9kh879gsnva7yb7ylw1&auth_token=x9d927uvd0vfv5ei5zo85d3wx8jw8e9n") >~
				{ _.getLines.mkString })

//{"type":"folder","id":"11450952","sequence_id":"6","etag":"6","name":"Test21","created_at":"2013-01-14T15:25:14-08:00","modified_at":"2013-01-14T16:54:14-08:00","description":"","size":7,"path_collection":{"total_count":2,"entries":[{"type":"folder","id":"0","sequence_id":null,"etag":null,"name":"All Files"},{"type":"folder","id":"11450946","sequence_id":"0","etag":"0","name":"Test2"}]},"created_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"modified_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"trashed_at":null,"purged_at":null,"owned_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"shared_link":null,"folder_upload_email":null,"parent":{"type":"folder","id":"11450946","sequence_id":"0","etag":"0","name":"Test2"},"item_status":"active","item_collection":{"total_count":1,"entries":[{"type":"file","id":"5003087120","sequence_id":"0","etag":"0","sha1":"fedd18797811a4af659678ea5db618f8dc91480b","name":"New Text Document.txt"}],"offset":0,"limit":100}}

		println(json)
		val folder = SJSON.in[Folder](json)
		println(folder)
//        val user = SJSON.in[User](json)
//
//        println(user.description)
//
//        val me = new User("Victor", "Me")
//        println(SJSON.toJSON(me))
//        println(SJSON.in[User](SJSON.toJSON(me)).description)
//
//
////        val users = new Users(List(new User("vic spivak", "It is me"), new User("vad spivak", "it is my son")).toArray)
//        val users = new Users(List(new User("vic spivak", "It is me"), new User("vad spivak", "it is my son")))
//
//        val out = SJSON.toJSON(users)
//
//        println(out)
//        val users1 = SJSON.in[Users](out)
//
//        println(users1)
    }
}
