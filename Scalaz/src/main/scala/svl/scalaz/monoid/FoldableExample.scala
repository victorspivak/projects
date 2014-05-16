package svl.scalaz.monoid

import scala.language.higherKinds
import scalaz.{Monoid, Foldable}
import scalaz.Scalaz._

object FoldableExample {
  def main(args: Array[String]) {
    def reduce[A](list:List[A])(implicit A:Monoid[A]) =
      list.foldLeft(A.zero)((a,b) => A.append(a, b))

    println(reduce(List(1,2,3,4,5)))

    println(reduce(List(List(1,2), List(3,4,5))))

    case class MyString(value:String)
    implicit val MyStringAsMonoid = new Monoid[MyString]{
      override def zero = MyString("")
      override def append(f1: MyString, f2: => MyString) = MyString(f1.value + f2.value)
    }
    println(reduce(List(MyString("Hello"), MyString(" "), MyString("World"))))

    def fold[F[_], A](fa: F[A])(implicit F: Foldable[F], A: Monoid[A]) : A =
      F.fold(fa)

    println(fold(List(List(1,2), List(3,4,5))))

  }
}
