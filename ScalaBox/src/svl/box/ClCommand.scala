package svl.box

import collection.mutable
import scala.Predef._
import scala.{Option, Some}
import sys.process._

class ClContext {
	val paths = new mutable.Stack[(String, String, Option[BoxFolder])]

	def reset() {
		paths.clear()
		push("root", "0", None)
	}

	def pop() {
		if (paths.size > 1)
			paths.pop()
	}

	def push(loc:(String, String, Option[BoxFolder])) = paths.push(loc)
	def current = paths.top
	def currentFolderId = current._2
	def updateCurrent(loc:(String, String, Option[BoxFolder])) = {paths.pop(); push(loc)}
	def resetCurrentFolder = {
		val loc = paths.pop()
		push((loc._1, loc._2, None))
	}

	def fullPath = paths.reverse.foldLeft("/")(_ + _._1 + "/")
}

abstract case class ClCommand(name:String, params:String)(implicit context:ClContext) {
	def execute()

	def getCurrentFolder = {
		var (parentName: String, parentId: String, parentFolder: Option[BoxFolder]) = context.current
		if (parentFolder.isEmpty) {
			parentFolder = BoxCommands.getFolder(parentId)
			context.updateCurrent((parentName, parentId, parentFolder))
		}

		parentFolder
	}

	def folderIdByName(name:String):Option[String] =
		getCurrentFolder.flatMap(_.items.entries.find(f => f.name == name && f.itemType == ItemType.folder)).map(_.id)

	def fileByName(name:String):Option[Item] =
		getCurrentFolder.flatMap(_.items.entries.find(f => f.name == name && f.itemType == ItemType.file))
}

class ExitCommand (implicit currentLoc:ClContext)  extends ClCommand("exit", "") {
	override def execute() {System.exit(0)}
}

class CdCommand (params:String) (implicit context:ClContext) extends ClCommand("cd", params) {
	override def execute() {cd(params)}

	def cd(path:String) {
		path match {
			case "" => return
			case "/" => context.reset()
			case p if p.startsWith("/") => cd ("/"); cd(p.substring(1))
			case restPath => val parts = path.split("/")
							parts.foreach(_ match {
								case ".." => context.pop()
								case part => folderIdByName(part) match {
									case Some(id) => context.push((part, id, None))
									case None => throw new FolderNotFoundException(part)
								}
							})
		}
	}
}

class LsCommand (params:String) (implicit context:ClContext) extends ClCommand("ls", params) {
	override def execute() {
		val format = "%-5s%-40s%10s\n"
		for (f <- getCurrentFolder) {
			printf("******* %s *******\n", f.name)
			f.items.entries.filter(_.itemType == ItemType.folder).sortWith(_.name < _.name).
					foreach(item => printf(format, "d", item.name, item.id))
			f.items.entries.filter(_.itemType == ItemType.file).sortWith(_.name < _.name).
					foreach(item => printf(format, "f", item.name, item.id))
		}
	}
}

class MkDirCommand (params:String) (implicit context:ClContext) extends ClCommand("mkdir", params) {
	override def execute() {
		BoxCommands.createFolder(params, context.currentFolderId)
		context.resetCurrentFolder
	}
}

class RmDirCommand (params:String) (implicit context:ClContext) extends ClCommand("rmdir", params) {
	override def execute() {
    folderIdByName(params) match {
      case Some(folderId) => BoxCommands.removeFolder(folderId)
                              context.resetCurrentFolder
      case None => throw new FolderNotFoundException(params)
    }
	}
}

class PutFileCommand (params:String) (implicit context:ClContext) extends ClCommand("putfile", params) {
	override def execute() {
    fileByName(params) match {
      case Some(item) => BoxCommands.updateFile(params, item.id, item.etag)
      case None => BoxCommands.createFile(params, context.currentFolderId)
    }

		context.resetCurrentFolder
	}
}

class GetFileCommand (params:String) (implicit context:ClContext) extends ClCommand("getfile", params) {
	override def execute() {
    getFile
	}

  def getFile:Option[String] = {
    fileByName(params) match {
      case Some(item) => BoxCommands.getFile(params, item.id); Some(params)
      case None => printf("The specified \'%s\' file is not found\n", params); None
    }
  }
}

class ViewFileCommand (params:String) (implicit context:ClContext) extends ClCommand("view", params) {
	override def execute() {
    new GetFileCommand(params).getFile.map(filename => """cmd /c "start %s"""".format(params).!)
	}
}

class ShellCommand (params:String) (implicit context:ClContext) extends ClCommand("!", params) {
	override def execute() {
    println(params !)
	}
}

class UnknownCommand (name:String, params:String) (implicit context:ClContext) extends ClCommand(name, params) {
	def execute() {println("Unknown Command>>>> " + name + "  " + params)}
}
