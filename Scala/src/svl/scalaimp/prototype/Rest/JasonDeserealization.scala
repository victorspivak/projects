package svl.scalaimp.prototype.Rest

import dispatch._
import scala.reflect.BeanInfo
import sjson.json.Serializer.SJSON
import util.parsing.json.JSON

object JasonDeserealization {
    def main(args: Array[String]) {
		@BeanInfo case class User(name:String, id:String, login:String) { def this() = this("", "", "") }
		@BeanInfo case class Item(itemType:String, name:String, id:String, size:Int,
			created_by:User, modified_by:User, item_collection:Items) { def this() = this("", "", "", 0, new User, new User, new Items) }
		@BeanInfo case class Items(total_count:Int, entries:List[Item]) { def this() = this(0, List()) }


        val json1 = """
  {"type":"folder","id":"11449764","sequence_id":"0","etag":"0","name":"Test1","created_at":"2013-01-10T12:08:17-08:00","modified_at":"2013-01-23T00:46:19-08:00",
  	"description":"","size":21,
  "path_collection":{"total_count":1,
  	"entries":[{"type":"folder","id":"0","sequence_id":null,"etag":null,"name":"All Files"}]},
  "created_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},
  "modified_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"trashed_at":null,"purged_at":null,
  "owned_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},
  "shared_link":null,"folder_upload_email":null,"parent":{"type":"folder","id":"0","sequence_id":null,"etag":null,"name":"All Files"},
  "item_status":"active",
  "item_collection":{"total_count":3,
  	"entries":[
  		{"type":"folder","id":"11452138","sequence_id":"0","etag":"0","name":"Test11"},
  		{"type":"file","id":"5002879826","sequence_id":"2","etag":"2","sha1":"fedd18797811a4af659678ea5db618f8dc91480b","name":"New Text Document - Copy.txt"},
  		{"type":"file","id":"5001312758","sequence_id":"3","etag":"3","sha1":"fedd18797811a4af659678ea5db618f8dc91480b","name":"New Text Document.txt"}],
	"offset":0,"limit":100}}"""

	val json = json1.replaceAll("\"type\"", "\"itemType\"")

//        val json = """{"type":"folder","id":"11449764","sequence_id":"0","etag":"0","name":"Test1","created_at":"2013-01-10T12:08:17-08:00","modified_at":"2013-01-23T00:46:19-08:00","description":"","size":21,"path_collection":{"total_count":1,"entries":[{"type":"folder","id":"0","sequence_id":null,"etag":null,"name":"All Files"}]},"created_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"modified_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"trashed_at":null,"purged_at":null,"owned_by":{"type":"user","id":"18214586","name":"Victor Spivak","login":"vspivak@box.com"},"shared_link":null,"folder_upload_email":null,"parent":{"type":"folder","id":"0","sequence_id":null,"etag":null,"name":"All Files"},"item_status":"active","item_collection":{"total_count":3,"entries":[{"type":"folder","id":"11452138","sequence_id":"0","etag":"0","name":"Test11"},{"type":"file","id":"5002879826","sequence_id":"2","etag":"2","sha1":"fedd18797811a4af659678ea5db618f8dc91480b","name":"New Text Document - Copy.txt"},{"type":"file","id":"5001312758","sequence_id":"3","etag":"3","sha1":"fedd18797811a4af659678ea5db618f8dc91480b","name":"New Text Document.txt"}],"offset":0,"limit":100}}
//					 	 				 	 				 |"""

		val folderObj = SJSON.in[Item](json)
		println(folderObj)

		val obj: Option[Any] = JSON.parseFull(json)
		println(obj)




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
