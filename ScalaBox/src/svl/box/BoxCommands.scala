package svl.box

import dispatch._
import classic.{:/, NoLogging, Http}
import dispatch.classic.mime._
import Mime._
import java.io.{FileOutputStream, File}

object BoxCommands {
	val server = "vspivak.inside-box.net"
	val apiVer = "api/2.0"
  val apiKey = "i8ei0dxlkwu8d3n9036trtt436u9kbsc"
  val clientSecret = "vBre9hlrrbmtxaCp5hPt7Ub3KN6m5eFU"

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

  def removeFolder(folderId:String) {
 		val http = new Http with NoLogging
 		val req = (:/(server) / apiVer / "folders" / folderId)
 		http((req.secure <:< authHeader).DELETE >| )

 		http.shutdown()
 	}

	def createFile(name:String, parentId:String) {
		val http = new Http with NoLogging

		val file = new File(name)
		val req = :/(server) / apiVer / "files" / "content"
		http(req.secure <:< authHeader << Map("parent_id" -> parentId) <<* (file.getName, file) >~ {_.getLines().mkString})

		http.shutdown()
	}

  def updateFile(name:String, fileId:String, etag:String) {
 		val http = new Http with NoLogging

 		val file = new File(name)
 		val req = :/(server) / apiVer / "files" / fileId / "content"
 		http(req.secure <:< (authHeader + ("If-Match" -> etag)) <<* (file.getName, file) >~ {_.getLines().mkString})

 		http.shutdown()
 	}

	def getFile(name:String, fileId:String) {
		val http = new Http with NoLogging

		val req = :/(server) / apiVer / "files" / fileId / "content"
    val out = new FileOutputStream(new File(name))
    http(req.secure <:< authHeader >>> out)
    out.close()
		http.shutdown()
	}
}
