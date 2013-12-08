package svl.learn.scala.classes

import scala.reflect.runtime.universe._

case class Foo[T](value:T)

object TypeTags {
    def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
        val targs = tag.tpe match { case TypeRef(_, _, args) => args }
        println(s"type of $x has type arguments $targs")
    }

    def getTypeTag[T: TypeTag](obj: T) = typeTag[T]

    def dump(symbol: Symbol)
    {
        println(">>>>>>>>>>>>>>>>>> " + symbol.asType.name)

    }

    def matchObj[T](x: T)(implicit tag: TypeTag[T]): Unit = {
        val targs = tag.tpe match { case TypeRef(_, _, args) => args }

//        targs match {
//            case List()
//        }
//        println(tag)
//        tag.tpe match {
//            case TypeRef(tagType, symbol, args) =>
//                println(tagType)
//                println(symbol)
//        }

        tag.tpe match {
//            case TypeRef(_, classOf[Int], args) =>
            case TypeRef(tagType, symbol, args) =>
                dump(symbol)
                println(tagType)
                println(symbol)
                println(args)
        }


        println(s"type of $x has type arguments $targs")
    }

    def main(args: Array[String]) {
//        paramInfo(42)
//        paramInfo(List(1, 2))
//        paramInfo(List("Hello", "World"))
//        paramInfo(List(Foo("Hello"), Foo("World")))

        matchObj(42)
        matchObj(List(1,2))

        val t = getTypeTag(42)
        println(t.tpe.declarations)
    }
}
