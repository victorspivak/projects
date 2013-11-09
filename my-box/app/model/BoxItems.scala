package model

import play.api.libs.json.JsValue
import model.BoxItem.BoxItemType

trait BoxItem extends BoxEntity {
  def parentId:String
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

case class BoxFolder(folderId: String, name: String, count:Int, size:Long, parentId:String) extends BoxItem {
  val id = folderId
  val itemType = BoxItemType.Folder
}

case class BoxFolderResource(id: String) extends BoxResource[BoxFolder] {
  val collectionPath:String = "/folders"
  val path = s"$collectionPath/$id"
  val getParams = List("fields" -> "id,name,item_collection,size,parent", "limit" -> "0")
}

class Json2BoxFolder extends json2Entity[BoxFolder] {
  def toEntity(json: JsValue): BoxFolder = {
    val count = (json \ "item_collection").asOpt[JsValue].flatMap{jvalue:JsValue =>
      (jvalue \ "total_count").asOpt[Int]}.getOrElse(0)
    val parentId = (json \ "parent").asOpt[JsValue].flatMap{jvalue:JsValue =>
      (jvalue \ "id").asOpt[String]}.getOrElse("")

    BoxFolder((json \ "id").as[String], (json \ "name").as[String], count, (json \ "size").asOpt[Long].getOrElse(0), parentId)
  }
}

case class BoxFile(fileId: String, name: String, size:Long, parentId:String) extends BoxItem {
  val id = fileId
  val itemType = BoxItemType.File
}

case class BoxFileResource(id: String) extends BoxResource[BoxFile] {
  val collectionPath:String = "/files"
  val path = s"$collectionPath/$id"
  val getParams = List("fields" -> "id,name,size,parent")
}

class Json2BoxFile extends json2Entity[BoxFile] {
  def toEntity(json: JsValue): BoxFile = {
    val parentId = (json \ "parent").asOpt[JsValue].flatMap{jvalue:JsValue =>
      (jvalue \ "id").asOpt[String]}.getOrElse("")
    BoxFile((json \ "id").as[String], (json \ "name").as[String], (json \ "size").asOpt[Long].getOrElse(0),parentId)
  }
}

case class BoxWebLink(webLinkId: String, name: String, parentId:String) extends BoxItem {
  val id = webLinkId
  val size = 0L
  val itemType = BoxItemType.WebLink
}

case class BoxWebLinkResource(id: String) extends BoxResource[BoxWebLink] {
  val collectionPath:String = "/files"
  val path = s"$collectionPath/$id"
  val getParams = List("fields" -> "id,name,size,parent")
}

class Json2BoxWebLink extends json2Entity[BoxWebLink] {
  def toEntity(json: JsValue): BoxWebLink = {
    val parentId = (json \ "parent").asOpt[JsValue].flatMap{jvalue:JsValue =>
      (jvalue \ "id").asOpt[String]}.getOrElse("")
    BoxWebLink((json \ "id").as[String], (json \ "name").as[String], parentId)
  }
}

case class BoxFolderItems(folderId: String, count:Int, items:List[BoxItem]) extends BoxEntity {
  val id = folderId
}

case class BoxFolderItemsResource(id: String) extends BoxResource[BoxFolderItems] {
  val collectionPath:String = "/folders"
  val path = s"$collectionPath/$id/items"
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

