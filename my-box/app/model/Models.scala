package model

import play.api.libs.json.JsValue

trait BoxEntity {
  def id:String
}

trait BoxResource[T <: BoxEntity] {
  def path:String
  def getParams:List[(String,String)]
}

trait json2Entity[T <: BoxEntity] {
  def toEntity(json: JsValue): T
}
