package lib

import play.api.mvc.RequestHeader
import controllers.commands._
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future

object CommandParser {
  def parseAsFuture(command:String)(implicit request:RequestHeader) = Future(parse(command))

  def parse(command:String)(implicit request:RequestHeader):BoxCommand = {
    BoxClient.logger.info(s"Parsing $command")
		val CommandTemplate = """\s*([!a-zA-Z0-9]*)\b*(.*)""".r
    command match {
			case CommandTemplate("cd", path) => new CdCommand(path.trim)
			case CommandTemplate("mkdir", name) => new MkDirCommand(name.trim)
			case CommandTemplate("rmdir", name) => new RmDirCommand(name.trim)
			case CommandTemplate("ls", "") | CommandTemplate("ll", "") => new ShowCurrentFolder
      case _ =>  throw new UnknownCommandException(command)
// 			case CommandTemplate("putfile", params) => new PutFileCommand(params.trim)
// 			case CommandTemplate("getfile", params) => new GetFileCommand(params.trim)
// 			case CommandTemplate("view", params) => new ViewFileCommand(params.trim)
//       case CommandTemplate("!", params) => new ShellCommand(params.trim)
// 			case CommandTemplate(name, params) => new UnknownCommand(name, params.trim)
		}
	}
}


