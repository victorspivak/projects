package svl.learn.scala.dynamic

import scala.language.dynamics

object TryDynamic {
  class Config(val map:Map[String, String]) extends Dynamic {
    def applyDynamic(methodName: String)(args: Any*) = {
      map.get(methodName)
    }
  }

  def main(args: Array[String]) {
    val config = new Config(Map("name" -> "My Name", "app" -> "My Application"))

    println(config.name())
    println(config.app())
    println(config.oops())
  }
}
