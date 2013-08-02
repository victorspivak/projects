package svl.scala.lib

object PerformanceUtil {
  def timer(func: => Unit) = {
    val start = System.currentTimeMillis()
    func
    System.currentTimeMillis() - start
  }

  def timerWithResult[T](func: => T):(T, Long) = {
    val start = System.currentTimeMillis()
    val res = func
    (res, System.currentTimeMillis() - start)
  }
}

class StopWatch(val startTime:Long) {
  def time = System.currentTimeMillis() - startTime
  def reset = StopWatch()
}

object StopWatch {
  def apply() = new StopWatch(System.currentTimeMillis())
}