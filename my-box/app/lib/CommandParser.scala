package lib

import play.api.mvc.RequestHeader
import controllers.commands.{BoxCommand, CdCommand, ShowCurrentFolder}
import play.api.libs.concurrent.Execution.Implicits._
import scala.concurrent.Future

object CommandParser {
  def parseAsFuture(command:String)(implicit request:RequestHeader) = Future(parse(command))

  def parse(command:String)(implicit request:RequestHeader):BoxCommand = {
    BoxClient.logger.info(s"Parsing $command")
		val CommandTemplate = """\s*([!a-zA-Z0-9]*)\b*(.*)""".r
    command match {
			case CommandTemplate("cd", params) => new CdCommand(params.trim)
			case CommandTemplate("ls", "") => new ShowCurrentFolder
      case _ =>  throw new UnknownCommandException(command)
//      case CommandTemplate("exit", params) => new ExitCommand
// 			case CommandTemplate("ls", params) => new LsCommand(params.trim)
// 			case CommandTemplate("ll", params) => new LsCommand(params.trim)
// 			case CommandTemplate("mkdir", params) => new MkDirCommand(params.trim)
// 			case CommandTemplate("rmdir", params) => new RmDirCommand(params.trim)
// 			case CommandTemplate("putfile", params) => new PutFileCommand(params.trim)
// 			case CommandTemplate("getfile", params) => new GetFileCommand(params.trim)
// 			case CommandTemplate("view", params) => new ViewFileCommand(params.trim)
//       case CommandTemplate("!", params) => new ShellCommand(params.trim)
// 			case CommandTemplate(name, params) => new UnknownCommand(name, params.trim)
		}
	}
}


