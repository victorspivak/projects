package svl.sbt.shell

import sbt._

object SbtShellPlugin extends Plugin {

  override lazy val settings = Seq(Keys.commands += shCommand)

  def shCommand = Command.args("sh", ("sh", "Executing Shell Command"), "Executing Shell Command: sh command [args]", "<shell command>") { (state, args) =>
    val ret = ("bash -c " + args.mkString(" ")) !

    state
  }
}

