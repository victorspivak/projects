package svl.box

object ItemType extends Enumeration {
	type ItemType = Value
	val folder, file, NONE = Value
}
import ItemType._

case class User(name: String, id: String) {
	def this() = this("", "")
}
case class Item(name: String, id: String, itemType: ItemType, etag:String) {
	def this() = this("", "", NONE, "0")
}
case class BoxFolder(name: String, id: String,
					 createdBy: User, modifiedBy: User,
					 items: Items) {
	def this() = this("", "", new User, new User, new Items)
	def findItemByName(name:String) = items.findItemByName(name)
}
case class Items(total_count: Int, entries: List[Item]) {
	def this() = this(0, List())
	lazy val indexByName = entries.map(i => (i.name, i)).toMap

	def findItemByName(name:String) = indexByName.get(name)
}
