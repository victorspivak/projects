package svl.scala.lib

import java.io.{OutputStream, InputStreamReader, BufferedReader}

object ConsoleUtil {
  private val rdr: BufferedReader = new BufferedReader(new InputStreamReader(System.in))

  def getUserInput(prompt: String) = {
    println(prompt)
    rdr.readLine
  }

  def printProgress(index:Long, maxCount:Long, step:Int, format:String = "Processed %8d / %d\n")
                   (implicit out:OutputStream = System.out) {
    if (index != 0 && (index % step) == 0) {
      Console.withOut(out)(printf(format, index, maxCount))
      out.flush()
    }
  }

  def printProgressWithTiming(index:Long, maxCount:Long, step:Int, stopWatch:StopWatch, format:String = "Processed %8d / %d timing %8d/%8d\n")
                             (implicit out:OutputStream = System.out) = {
    if (index != 0 && (index % step) == 0) {
      Console.withOut(out)(printf(format, index, maxCount, stopWatch.time, stopWatch.time/step))
      out.flush()
      stopWatch.reset
    }
    else
      stopWatch
  }

  def printTiming(index:Long, maxCount:Long, step:Int, stopWatch:StopWatch, format:String = "Processed %8d / %d timing %8d/%8d\n")
                             (implicit out:OutputStream = System.out) {
    Console.withOut(out)(printf(format, index, maxCount, stopWatch.time, stopWatch.time/step))
    out.flush()
  }
}
