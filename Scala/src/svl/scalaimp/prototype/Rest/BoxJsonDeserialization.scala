package svl.scalaimp.prototype.Rest

import scala.reflect.BeanInfo
import sjson.json.Serializer.SJSON
import util.parsing.json.JSON

object BoxJsonDeserialization {
    def main(args: Array[String]) {
        class CC[T] {
            def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])
        }

        object M extends CC[Map[String, Any]]
        object L extends CC[List[Any]]
        object S extends CC[String]
        object D extends CC[Double]
        object I extends CC[Int] {
            override def unapply(a: Any): Option[Int] = Some(a.asInstanceOf[Double].toInt)
        }
        object B extends CC[Boolean]

        object ItemType extends Enumeration {
            type ItemType = Value
            val folder, file, NONE = Value
        }
        import ItemType._

        case class User(name: String, id: String) {
            def this() = this("", "")
        }
        case class Item(name: String, id: String, itemType:ItemType) {
            def this() = this("", "", NONE)
        }
        case class BoxFolder(name: String, id: String,
                        created_by: User, modified_by: User, item_collection: Items) {
            def this() = this("", "", new User, new User, new Items)
        }
        case class Items(total_count: Int, entries: List[Item]) {
            def this() = this(0, List())
        }

//        case class BoxFile(itemType: String, name: String, id: String, size: Int,
//                        created_by: User, modified_by: User, item_collection: Items) {
//            def this() = this("", "", "", 0, new User, new User, new Items)
//        }

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

        val parsed = JSON.parseFull(json)
        println(parsed)

        val res = for {
            Some(M(mainItem)) <- List(parsed)
            S(mainItemId) = mainItem("id")
            S(mainItemName) = mainItem("name")
            M(createdBy) = mainItem("created_by")
            S(createdByName) = createdBy("name")
            S(createdById) = createdBy("id")
            M(modifiedBy) = mainItem("modified_by")
            S(modifiedByName) = modifiedBy("name")
            S(modifiedById) = modifiedBy("id")

            M(itemsCollection) = mainItem("item_collection")
            I(total_count) = itemsCollection("total_count")
            L(entries) = itemsCollection("entries")
            L(items) = for {
                M(item) <- entries
                S(itemType) = item("itemType")
                S(itemId) = item("id")
                S(itemName) = item("name")
            } yield{
                new Item(itemName, itemId, ItemType.withName(itemType))
            }

        } yield {
            new BoxFolder(mainItemName, mainItemId, new User(createdByName, createdById), new User(modifiedByName, modifiedById),
                new Items(total_count, items.asInstanceOf[List[Item]]))
        }

        println(res)
    }
}
