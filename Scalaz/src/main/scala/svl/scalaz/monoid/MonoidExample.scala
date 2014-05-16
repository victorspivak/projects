package svl.scalaz.monoid

import scala.language.higherKinds
import scalaz.Monoid
import scalaz.Scalaz._

object MonoidExample {
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

    //Below is a naive implementation of Foldable
    //See FoldableExample to check how Foldable works from scalaz
    trait Foldable[F[_]] {
      def fold[A : Monoid](fa: F[A]) : A
    }

    implicit val ListHasFoldable = new Foldable[List] {
      override def fold[A](fa: List[A])(implicit A:Monoid[A]) = {
        fa.foldLeft(A.zero) ((x,y) => A.append(x,y))
      }
    }

    def fold[F[_], A](fa: F[A])(implicit F: Foldable[F], A: Monoid[A]) : A =
      F.fold(fa)

    println(fold(List(List(1,2), List(3,4,5))))

  }
}
