package svl.box

import dispatch._
import util.parsing.json.JSON

class CC[T] {def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])}
object M extends CC[Map[String, Any]]
object L extends CC[List[Any]]
object S extends CC[String]
object D extends CC[Double]
object I extends CC[Int] {override def unapply(a: Any): Option[Int] = Some(a.asInstanceOf[Double].toInt)}
object B extends CC[Boolean]

object JsonParser {
	def parseFolder(json: String): Option[BoxFolder] = {
		val parsed = JSON.parseFull(json)

//		"path_collection" : { "entries" : [ { "etag" : null,
//		            "id" : "0",
//		            "name" : "All Files",
//		            "sequence_id" : null,
//		            "type" : "folder"
//		          },
//		          { "etag" : "0",
//		            "id" : "11449764",
//		            "name" : "Test1",
//		            "sequence_id" : "0",
//		            "type" : "folder"
//		          }
//		        ],
//		      "total_count" : 2
//		    },


		val res = for {
			Some(M(mainItem)) <- Some(parsed)
			S(mainItemId) = mainItem("id")
			S(mainItemName) = mainItem("name")
		} yield {
			new BoxFolder(mainItemName, mainItemId,
				extractUser(mainItem("created_by")),
				extractUser(mainItem("modified_by")),
				extractItems(mainItem("item_collection")))
		}
		res
	}

	def extractUser(item:Any) = {
		val userObj = for {
			M(user) <- Some(item.asInstanceOf[Map[String, Any]])
			S(name) = user("name")
			S(id) = user("id")
		} yield {
			new User(name, id)
		}
		userObj.getOrElse(new User)
	}

	def extractItems(itemsCollection:Any) = {
		val extracted = for {
			M(itemsCollection) <- Some(itemsCollection.asInstanceOf[Map[String, Any]])
			I(totalCount) = itemsCollection("total_count")
			L(entries) = itemsCollection("entries")
			items = for {
				M(item) <- entries
				S(itemType) = item("type")
				S(itemId) = item("id")
				S(itemName) = item("name")
			} yield{
				new Item(itemName, itemId, ItemType.withName(itemType))
			}
		} yield {
			new Items(totalCount, items.asInstanceOf[List[Item]])
		}

		extracted.getOrElse(new Items)
	}
}
