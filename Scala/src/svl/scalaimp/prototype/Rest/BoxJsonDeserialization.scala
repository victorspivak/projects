package svl.scalaimp.prototype.Rest

import scala.reflect.BeanInfo
import sjson.json.Serializer.SJSON
import util.parsing.json.JSON
import dispatch.{:/, Http}

object BoxJsonDeserialization {
//	def main(args: Array[String]) {
//		class CC[T] {
//			def unapply(a: Any): Option[T] = Some(a.asInstanceOf[T])
//		}
//
//		object M extends CC[Map[String, Any]]
//		object L extends CC[List[Any]]
//		object S extends CC[String]
//		object D extends CC[Double]
//		object I extends CC[Int] {
//			override def unapply(a: Any): Option[Int] = Some(a.asInstanceOf[Double].toInt)
//		}
//		object B extends CC[Boolean]
//
//		object ItemType extends Enumeration {
//			type ItemType = Value
//			val folder, file, NONE = Value
//		}
//		import ItemType._
//
//		case class User(name: String, id: String) {
//			def this() = this("", "")
//		}
//		case class Item(name: String, id: String, itemType: ItemType) {
//			def this() = this("", "", NONE)
//		}
//		case class BoxFolder(name: String, id: String,
//							 created_by: User, modified_by: User, item_collection: Items) {
//			def this() = this("", "", new User, new User, new Items)
//		}
//		case class Items(total_count: Int, entries: List[Item]) {
//			def this() = this(0, List())
//		}
//
//		val http = new Http
//		val req = :/("vspivak.inside-box.net") / "api/2.0/folders/11449764"
//		val json = http(req.secure <:< Map("Authorization" -> "BoxAuth api_key=3i8u7632f52ye9kh879gsnva7yb7ylw1&auth_token=x9d927uvd0vfv5ei5zo85d3wx8jw8e9n") >~ {
//			_.getLines.mkString
//		})
//
//		val parsed = JSON.parseFull(json)
//		println(parsed)
//
//        val res = for {
//            Some(M(mainItem)) <- List(parsed)
//            S(mainItemId) = mainItem("id")
//            S(mainItemName) = mainItem("name")
//            M(createdBy) = mainItem("created_by")
//            S(createdByName) = createdBy("name")
//            S(createdById) = createdBy("id")
//            M(modifiedBy) = mainItem("modified_by")
//            S(modifiedByName) = modifiedBy("name")
//            S(modifiedById) = modifiedBy("id")
////
////            M(itemsCollection) = mainItem("item_collection")
////            I(total_count) = itemsCollection("total_count")
////            L(entries) = itemsCollection("entries")
////            L(items) = for {
////                M(item) <- entries
////                S(itemType) = item("itemType")
////                S(itemId) = item("id")
////                S(itemName) = item("name")
////            } yield{
////                new Item(itemName, itemId, ItemType.withName(itemType))
////            }
////
//        } yield {
//            new BoxFolder(mainItemName, mainItemId, new User(createdByName, createdById), new User(modifiedByName, modifiedById),
//                new Items())
////            new BoxFolder(mainItemName, mainItemId, new User(createdByName, createdById), new User(modifiedByName, modifiedById),
////                new Items(total_count, items.asInstanceOf[List[Item]]))
//        }
//
//		println(res)
//	}
}
