package svl.box

object CL {
	implicit val clContext:ClContext = new ClContext
	clContext.reset()

	def main(args: Array[String]) {
		while(true) {
			try {
				nextCommand.execute()
			} catch {
				case e:FolderNotFoundException => println("*** ERROR ***   \'" + e.getMessage + "\' not found")
			}
		}
	}

	def nextCommand = {
		val CommandTemplate = """\s*([!a-zA-Z0-9]*)\b*(.*)""".r

		printf("%s $ ", clContext.fullPath)
		val line = Console.readLine()
		line match {
			case CommandTemplate("exit", params) => new ExitCommand
			case CommandTemplate("cd", params) => new CdCommand(params.trim)
			case CommandTemplate("ls", params) => new LsCommand(params.trim)
			case CommandTemplate("ll", params) => new LsCommand(params.trim)
			case CommandTemplate("mkdir", params) => new MkDirCommand(params.trim)
			case CommandTemplate("rmdir", params) => new RmDirCommand(params.trim)
			case CommandTemplate("putfile", params) => new PutFileCommand(params.trim)
			case CommandTemplate("getfile", params) => new GetFileCommand(params.trim)
			case CommandTemplate("view", params) => new ViewFileCommand(params.trim)
      case CommandTemplate("!", params) => new ShellCommand(params.trim)
			case CommandTemplate(name, params) => new UnknownCommand(name, params.trim)
		}
	}
}
