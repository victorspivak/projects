package svl.learn.scala.classes

import scala.reflect.runtime.universe._

object Classes{
    case class Foo[T](value:T)

    def factory():AnyRef = Foo("Hello")

    def main(args: Array[String]) {
        val v = factory()

        testObj(v)
    }

    def testObj[T](v: AnyRef) (implicit tag: TypeTag[T]){
        println(tag.tpe)
        v match {
            case s: Foo[Int] => println(s"Got Int ${s.value}")
            case s: Foo[String] => println(s"Got String ${s.value}")
        }
    }
}
