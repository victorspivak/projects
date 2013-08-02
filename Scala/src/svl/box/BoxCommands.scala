package svl.box

import dispatch._
//import dispatch.mime._
//import Mime._
import java.io.File

object BoxCommands {
	val server = "vspivak.inside-box.net"
	val apiVer = "api/2.0"
	val authHeader = Map("Authorization" -> "BoxAuth api_key=3i8u7632f52ye9kh879gsnva7yb7ylw1&auth_token=x9d927uvd0vfv5ei5zo85d3wx8jw8e9n")

	def getFolder(id:String) = {
		val http = new Http with NoLogging
		val req = :/(server) / apiVer / "folders" / id
		val json = http(req.secure <:< authHeader >~ {_.getLines().mkString})

		http.shutdown()
		JsonParser.parseFolder(json)
	}

	def createFolder(name:String, parentId:String) = {
		val payload = """{"name":"%s", "parent": {"id": "%s"}}""".format(name, parentId)
		val http = new Http with NoLogging
		val req = :/(server) / apiVer / "folders"
		val json = http(req.secure <:< authHeader << payload >~ {_.getLines().mkString})

		http.shutdown()
		JsonParser.parseFolder(json)
	}

	def createFile(name:String, parentId:String) {
////		val http = new Http with NoLogging
//		val http = new Http
//
//		val file = new File(name)
//		val req = :/(server) / apiVer / "files" / "content"
//		val json = http(req.secure <:< authHeader << file >~ {_.getLines().mkString})
//
//		println(json)
//		http.shutdown()
	}
}
