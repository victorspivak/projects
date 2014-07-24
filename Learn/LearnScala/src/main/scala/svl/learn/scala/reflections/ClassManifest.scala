package svl.learn.scala.reflections

import scala.reflect.Manifest

//implicit m:Manifest[T]

object ClassManifest extends App{
  def analyze[T](list:List[T])(implicit m:Manifest[T]) = {
    m.toString() match {
      case "java.lang.String" => println("It is list of ints")
      case "Int" => println("It is list of strings")
      case _ @clazz => println("It is list of unknowns - " + clazz)
    }
  }

  analyze(List("Hello"))
  analyze(List(1))
}
