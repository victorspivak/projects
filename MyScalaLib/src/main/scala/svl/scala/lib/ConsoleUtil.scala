package svl.scala.lib

import java.io.{InputStreamReader, BufferedReader}

object ConsoleUtil {
  private val rdr: BufferedReader = new BufferedReader(new InputStreamReader(System.in))
  def getUserInput(prompt: String) = {
    println(prompt)
    rdr.readLine
  }
}
