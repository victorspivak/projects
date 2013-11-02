package model

import play.api.libs.json.JsValue
import model.BoxItem.BoxItemType

trait BoxItem extends BoxEntity {
  def id:String
  def name:String
  def size:Long
  def itemType:BoxItemType.BoxItemType
}

object BoxItem {
  object BoxItemType extends Enumeration{
    type BoxItemType = Value
    val Folder, File, WebLink = Value
  }
}

case class BoxFolder(folderId: String, name: String, count:Int, size:Long) extends BoxItem {
  val id = folderId
  val itemType = BoxItemType.Folder
}

case class BoxFolderResource(id: String) extends BoxResource[BoxFolder] {
  val path = s"/folders/$id"
  val getParams = List("fields" -> "id,name,item_collection,size", "limit" -> "0")
}

class Json2BoxFolder extends json2Entity[BoxFolder] {
  def toEntity(json: JsValue): BoxFolder = {
    val count:Int = (json \ "item_collection").asOpt[JsValue].flatMap{jvalue:JsValue =>
      (jvalue \ "total_count").asOpt[Int]}.getOrElse(0)

    BoxFolder((json \ "id").as[String], (json \ "name").as[String], count, (json \ "size").asOpt[Long].getOrElse(0))
  }
}

case class BoxFile(fileId: String, name: String, size:Long) extends BoxItem {
  val id = fileId
  val itemType = BoxItemType.File
}

case class BoxFileResource(id: String) extends BoxResource[BoxFile] {
  val path = s"/files/$id"
  val getParams = List("fields" -> "id,name,size")
}

class Json2BoxFile extends json2Entity[BoxFile] {
  def toEntity(json: JsValue): BoxFile = {
    BoxFile((json \ "id").as[String], (json \ "name").as[String], (json \ "size").asOpt[Long].getOrElse(0))
  }
}

case class BoxWebLink(webLinkId: String, name: String) extends BoxItem {
  val id = webLinkId
  val size = 0L
  val itemType = BoxItemType.WebLink
}

case class BoxWebLinkResource(id: String) extends BoxResource[BoxWebLink] {
  val path = s"/files/$id"
  val getParams = List("fields" -> "id,name,size")
}

class Json2BoxWebLink extends json2Entity[BoxWebLink] {
  def toEntity(json: JsValue): BoxWebLink = {
    BoxWebLink((json \ "id").as[String], (json \ "name").as[String])
  }
}

case class BoxFolderItems(folderId: String, count:Int, items:List[BoxItem]) extends BoxEntity {
  val id = folderId
}

case class BoxFolderItemsResource(id: String) extends BoxResource[BoxFolderItems] {
  val path = s"/folders/$id/items"
  val getParams = List("fields" -> "id,name,type,size", "limit" -> "1000")
}

class Json2BoxFolderItems(folderId:String) extends json2Entity[BoxFolderItems] {
  val fileConvertor = new Json2BoxFile
  val folderConvertor = new Json2BoxFolder
  val webLinkConvertor = new Json2BoxWebLink

  def toItem(json: JsValue): BoxItem = {
    (json \ "type").as[String] match {
      case "file" => fileConvertor.toEntity(json)
      case "folder" => folderConvertor.toEntity(json)
      case "web_link" => webLinkConvertor.toEntity(json)
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

