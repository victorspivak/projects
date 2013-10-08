package model

import play.api.libs.json.JsValue

case class BoxUser(userId: String, name: String, login:String, status:String) extends BoxEntity {
  def id = userId
}

case class BoxUserResource(id: String) extends BoxResource[BoxUser] {
  val path = s"/users/$id"
  def getParams = List("fields" -> "id,name,login,status")
}

class Json2BoxUser extends json2Entity[BoxUser] {
  def toEntity(json: JsValue): BoxUser = {
    BoxUser((json \ "id").as[String], (json \ "name").as[String], (json \ "login").as[String], (json \ "status").as[String])
  }
}

