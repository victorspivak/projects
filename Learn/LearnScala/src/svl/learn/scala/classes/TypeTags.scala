package svl.learn.scala.classes

import scala.reflect.runtime.universe._

object TypeTags {
    def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
        val targs = tag.tpe match { case TypeRef(_, _, args) => args }
        println(s"type of $x has type arguments $targs")
    }

    def main(args: Array[String]) {
        paramInfo(42)
        paramInfo(List(1, 2))
    }
}
