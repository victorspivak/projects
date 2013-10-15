package lib

import controllers.commands.ShowFolder
import play.api.mvc.{Request, AnyContent}

object CommandParser {
  def parse(command:String)(implicit request:Request[AnyContent]) = {
		val CommandTemplate = """\s*([!a-zA-Z0-9]*)\b*(.*)""".r

    command match {
			case CommandTemplate("cd", params) => new ShowFolder(params.trim)
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

