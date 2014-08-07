package svl.learn.scala.reflections

import scala.reflect.api.TypeTags
import scala.reflect.runtime.universe._

/*
 * User: victor    Date: 8/6/14   Time: 7:04 PM
 */
object TypeTagsExample extends App {
  def analyze[T](list:List[T])(implicit tag:TypeTag[T]) = {
    typeOf[T] match {
      case t if t =:= typeOf[Int] => println("It is list of ints")
      case t if t =:= typeOf[String] => println("It is list of strings")
      case _ @clazz => println("It is list of unknowns - " + clazz)
    }
  }

  analyze(List("Hello"))
  analyze(List(1))
}
