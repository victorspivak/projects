package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.JsValue
import models.WebSocketRobot

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def learnWebSocket = Action {  implicit request  =>
    Ok(views.html.websocket("Learn Web Socket."))
  }

  def startWebSocket = WebSocket.async[JsValue] { request  =>
    WebSocketRobot.start()
  }
}