package model

import play.api.libs.json.JsValue

case class BoxUser(userId: String, name: String, login:String, status:String) extends BoxEntity {
  val id = userId
}

case class BoxUserResource(id: String) extends BoxResource[BoxUser] {
  val collectionPath:String = "/users"
  val path = s"$collectionPath/$id"
  val getParams = List("fields" -> "id,name,login,status")
}

class Json2BoxUser extends json2Entity[BoxUser] {
  def toEntity(json: JsValue): BoxUser = {
    BoxUser((json \ "id").as[String], (json \ "name").as[String], (json \ "login").as[String], (json \ "status").as[String])
  }
}

