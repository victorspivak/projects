package model

import play.api.libs.json.JsValue
import model.BoxItem.BoxItemType

trait BoxItem extends BoxEntity {
  def name:String
  def itemType:BoxItemType.BoxItemType
}

object BoxItem {
  object BoxItemType extends Enumeration{
    type BoxItemType = Value
    val Folder, File = Value
  }
}

case class BoxFolder(folderId: String, name: String, count:Int) extends BoxItem {
  def id = folderId
  def itemType = BoxItemType.Folder
}

case class BoxFolderResource(id: String) extends BoxResource[BoxFolder] {
  val path = s"/folders/$id"
  def getParams = List("fields" -> "id,name,item_collection", "limit" -> "0")
}

class Json2BoxFolder extends json2Entity[BoxFolder] {
  def toEntity(json: JsValue): BoxFolder = {
    val count:Int = (json \ "item_collection").asOpt[JsValue].flatMap{jvalue:JsValue =>
      (jvalue \ "total_count").asOpt[Int]}.getOrElse(0)

    BoxFolder((json \ "id").as[String], (json \ "name").as[String], count)
  }
}

case class BoxFile(fileId: String, name: String) extends BoxItem {
  def id = fileId
  def itemType = BoxItemType.File
}

case class BoxFileResource(id: String) extends BoxResource[BoxFile] {
  val path = s"/files/$id"
  def getParams = List("fields" -> "id,name")
}

class Json2BoxFile extends json2Entity[BoxFile] {
  def toEntity(json: JsValue): BoxFile = {
    BoxFile((json \ "id").as[String], (json \ "name").as[String])
  }
}

case class BoxFolderItems(folderId: String, count:Int, items:List[BoxItem]) extends BoxEntity {
  def id = folderId
}

case class BoxFolderItemsResource(id: String) extends BoxResource[BoxFolderItems] {
  val path = s"/folders/$id/items"
  def getParams = List("fields" -> "id,name,type")
}

class Json2BoxFolderItems(folderId:String) extends json2Entity[BoxFolderItems] {
  val fileConvertor = new Json2BoxFile
  val folderConvertor = new Json2BoxFolder

  def toItem(json: JsValue): BoxItem = {
    (json \ "type").as[String] match {
      case "file" => fileConvertor.toEntity(json)
      case "folder" => folderConvertor.toEntity(json)
    }
  }

  def toEntity(json: JsValue): BoxFolderItems = {
    val count = (json \ "total_count").as[Int]
    val jsonListOfItems = (json \ "entries").as[List[JsValue]]

    val listOfItems =
      for(jsonItem <- jsonListOfItems) yield toItem(jsonItem)

    BoxFolderItems(folderId, count, listOfItems)
  }
}

